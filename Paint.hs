{-# LANGUAGE OverloadedStrings #-}
module Paint where

import System.IO
import Data.ByteString.Builder
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Internal as BSI
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Control.Monad
import Data.List
import Foreign.Storable
import Data.IORef
import Data.Bits
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import Codec.Picture
import Control.Concurrent.STM
import Control.Concurrent

import Rect
import R2

type Color  = (Int,Int,Int)
type Pixmap = Image PixelRGB8
type Painter = [Paint] -> IO ()

data Paint =
  Fill (Rect ()) Color |
  Box (Rect ()) Color |
  Line R2 R2 Color |
  Blit R2 Pixmap |
  Upload Int Pixmap |
  PutImage R2 Int |
  Label R2 Text |
  FilePicker |
  Copy Text |
  SetCursor Cursor

showPaint :: Paint -> String
showPaint p = case p of
  Fill r c -> unwords ["Fill", show r, show c]
  Box r c -> unwords ["Box", show r, show c]
  Line x0 x1 c -> unwords ["Line", show x0, show x1, show c]
  Blit x pix -> unwords ["Blit", show x, "?"]
  Upload n pix -> "?"
  PutImage x n -> "?"
  Label x txt -> "?"
  FilePicker -> "FilePicker"
  Copy txt -> "Copy " ++ show txt
  SetCursor c -> "SetCursor " ++ show c

data Cursor =
  CursorDefault |
  CursorOpenHand |
  CursorClosedHand |
  CursorPointing |
  CursorText |
  CursorLeftRight |
  CursorUpDown
    deriving (Eq, Show)

asciiHex :: Word8 -> Word8
asciiHex n = if n < 10
  then n + 0x30
  else n + 0x57

{-
encodeDataInHex :: Vector Word8 -> ByteString
encodeDataInHex bytes =
  let len = V.length bytes in
  BSI.unsafeCreate (2*len) $ \ptr -> do
    posRef <- newIORef 0
    V.forM_ bytes $ \w8 -> do
      i <- readIORef
      let nib1 = asciiHex ((w8 .&. 0xf0) `shiftR` 4)
      let nib0 = asciiHex (w8 .&. 0x0f) 
      pokeElemOff ptr i nib1
      pokeElemOff ptr (i+1) nib0
      modifyIORef posRef (+2)
-}

encodeDataInHex :: Vector Word8 -> ByteString
encodeDataInHex v =
  let bytes = V.toList v in
  let f b = [ asciiHex ((b .&. 0xf0) `shiftR` 4)
            , asciiHex  (b .&. 0x0f) ] in
  let nibbles = concatMap f bytes in
  BSL.pack nibbles

pix :: Pixmap
pix = Image 2 2 $ V.fromList
  [0xff,0x00,0x00
  ,0x00,0xff,0x00
  ,0xab,0xcd,0xef
  ,0x01,0x02,0x03]

encodePixmap :: Pixmap -> Builder
encodePixmap (Image w h vectorWord8) =
  intDec w <> space <>
  intDec h <> space <>
  lazyByteString (encodeDataInHex vectorWord8)

encodeCursor :: Cursor -> Builder
encodeCursor c = case c of
  CursorDefault -> "default"
  CursorOpenHand -> "open-hand"
  CursorClosedHand -> "closed-hand"
  CursorPointing -> "pointing"
  CursorText -> "text"
  CursorLeftRight -> "left-right"
  CursorUpDown -> "up-down"

space :: Builder
space = word8 0x20

newline :: Builder
newline = word8 0x0a

sp2nl :: Char -> Char
sp2nl '\n' = ' '
sp2nl x = x

encodeRect :: Rect a -> Builder
encodeRect (Rect _ l t r b) =
  intDec (round l) <> space <>
  intDec (round t) <> space <>
  intDec (round (r-l)) <> space <>
  intDec (round (b-t))

encodeRectF :: Rect a -> Builder
encodeRectF (Rect _ l t r b) =
  doubleDec l <> space <>
  doubleDec t <> space <>
  doubleDec (r-l) <> space <>
  doubleDec (b-t)

encodeRGB :: Color -> Builder
encodeRGB (r,g,b) = intDec r <> space <> intDec g <> space <> intDec b

encodeR2 :: R2 -> Builder
encodeR2 (x,y) = intDec (floor x) <> space <> intDec (floor y)
  
encodePaintCommand :: Paint -> Builder
encodePaintCommand p = mconcat (intersperse space words) where
  words = case p of
    Line xy1 xy2 rgb -> ["line", encodeR2 xy1, encodeR2 xy2, encodeRGB rgb]
    Fill r rgb -> ["fill", encodeRectF r, encodeRGB rgb]
    Box r rgb -> ["box", encodeRect r, encodeRGB rgb]
    Blit xy img -> ["blit", encodeR2 xy, encodePixmap img]
    Upload n img -> ["upload", intDec n, encodePixmap img]
    PutImage xy n -> ["image" , encodeR2 xy, intDec n]
    Label xy s ->
      ["label", encodeR2 xy, (byteString . encodeUtf8 . T.map sp2nl) s]
    FilePicker -> ["file-picker"]
    Copy s -> ["copy", byteString (encodeUtf8 s)]
    SetCursor c -> ["cursor"]

compilePaintCommands :: [Paint] -> Builder
compilePaintCommands ps =
  let encode p = encodePaintCommand p <> newline in
  mconcat (map encode ps) 

newPaintOut :: Handle -> [Paint] -> IO ()
newPaintOut h x = (hPutBuilder h . compilePaintCommands) x

hPaintOut = newPaintOut

{-
translatePaint :: Paint -> Z2 -> Paint
translatePaint p delta = case p of
  Fill r c -> Fill (r `translateRect` delta) c
  Box r c -> Box (r `translateRect` delta) c
  Line x0 x1 c -> Line (x0 .+. delta) (x1 .+. delta) c
  Blit x pix -> Blit (x .+. delta) pix
  PutImage x n -> PutImage (x .+. delta) n
  Label x t -> Label (x .+. delta) t
  other -> other
-}

-- we have a thread throttling the paint flushing
newPaintWorker :: Handle -> IO ([Paint] -> IO ())
newPaintWorker h = do
  ch <- atomically newTChan
  tv <- atomically (newTVar False)
  forkIO (flusher tv ch)
  forkIO (painter h ch)
  return $ \cmds -> atomically $ do
    writeTChan ch (Just cmds)
    writeTVar tv True

painter :: Handle -> TChan (Maybe [Paint]) -> IO a
painter h ch = forever $ do
  m <- atomically (readTChan ch)
  case m of
    Nothing -> hPutStrLn h "flush"
    Just cmds -> hPaintOut h cmds

flusher :: TVar Bool -> TChan (Maybe [Paint]) -> IO a
flusher tv ch = forever $ do
  atomically $ do
    x <- readTVar tv
    when (x == False) retry
    writeTChan ch Nothing
    writeTVar tv False
  threadDelay 16000
