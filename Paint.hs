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

type Color  = (Int,Int,Int)
type Pixmap = Image PixelRGB8

data Rect = Rect
 { rectLeft   :: Int
 , rectTop :: Int
 , rectWidth  :: Int
 , rectHeight :: Int }
   deriving (Show)

data XY = XY Int Int deriving Show

data Paint =
  Fill Rect Color |
  Box Rect Color |
  Line XY XY Color |
  Blit XY Pixmap |
  Upload Int Pixmap |
  PutImage XY Int |
  Label XY Text |
  FilePicker |
  Copy Text |
  SetCursor Cursor

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
    Line (XY x y) (XY u v) (r,g,b) -> "line" : map intDec [x,y,u,v,r,g,b]
    Fill (Rect x y w h) (r,g,b)    -> "fill" : map intDec [x,y,w,h,r,g,h]
    Box (Rect x y w h) (r,g,b)     -> "box"  : map intDec [x,y,w,h,r,g,h]
    Blit (XY x y) img -> ["blit", intDec x, intDec y, encodePixmap img]
    Upload n img -> ["upload", intDec n, encodePixmap img]
    PutImage (XY x y) n -> ["image", intDec x, intDec y, intDec n]
    Label (XY x y) s ->
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
