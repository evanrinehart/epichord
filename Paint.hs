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
import Data.List
import Foreign.Storable
import Data.IORef
import Data.Bits
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Data.Word
import Codec.Picture

import Rect
import R2

type Color  = (Int,Int,Int)
type Pixmap = Image PixelRGB8
type Painter = [Paint] -> IO ()

data Paint =
  Fill (Rect Int) Color |
  Box (Rect Int) Color |
  Line Z2 Z2 Color |
  Blit Z2 Pixmap |
  Upload Int Pixmap |
  PutImage Z2 Int |
  Label Z2 Text |
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
    deriving Show

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
  
encodePaintCommand :: Paint -> Builder
encodePaintCommand p = mconcat (intersperse space words) where
  words = case p of
    Line (x,y) (u,v) (r,g,b) -> "line" : map intDec [x,y,u,v,r,g,b]
    Fill (Rect x y w h) (r,g,b)    -> "fill" : map intDec [x,y,w,h,r,g,h]
    Box (Rect x y w h) (r,g,b)     -> "box"  : map intDec [x,y,w,h,r,g,h]
    Blit (x,y) img -> ["blit", intDec x, intDec y, encodePixmap img]
    Upload n img -> ["upload", intDec n, encodePixmap img]
    PutImage (x,y) n -> ["image", intDec x, intDec y, intDec n]
    Label (x,y) s ->
      ["label", intDec x, intDec y, byteString . encodeUtf8 . T.map sp2nl $ s]
    FilePicker -> ["file-picker"]
    Copy s -> ["copy", byteString (encodeUtf8 s)]
    SetCursor c -> ["cursor"]

compilePaintCommands :: [Paint] -> Builder
compilePaintCommands ps =
  let encode p = encodePaintCommand p <> newline <> "flush\n" in
  mconcat (map encode ps) 

newPaintOut :: Handle -> [Paint] -> IO ()
newPaintOut h = hPutBuilder h . compilePaintCommands

translatePaint :: Paint -> Z2 -> Paint
translatePaint p delta = case p of
  Fill r c -> Fill (r `translateRect` delta) c
  Box r c -> Box (r `translateRect` delta) c
  Line x0 x1 c -> Line (x0 .+. delta) (x1 .+. delta) c
  Blit x pix -> Blit (x .+. delta) pix
  PutImage x n -> PutImage (x .+. delta) n
  Label x t -> Label (x .+. delta) t
  other -> other

