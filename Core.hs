module Main where

import Debug.Trace (trace)
import Control.Concurrent
import System.IO
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.List
import Data.Ord
import Data.Bits
import Data.Word
import Data.Monoid
import Data.Maybe
import Control.Monad
import Numeric
import Data.Functor

import ZMidi.Core

dumpMidiFile :: String -> String -> MidiFile -> IO ()
dumpMidiFile n1 n2 smf = do
  let p1 = "/tmp/epichord-XYZW/voicedump-" ++ n1
  let p2 = "/tmp/epichord-XYZW/tempodump-" ++ n2
  let chunks1 = uncollateVoiceEvents smf :: [ByteString]
--  putStrLn ((unlines . map showChunk1) chunks1)
  let chunks2 = uncollateTempoChanges smf
--  putStrLn ((unlines . map showChunk2) chunks2)
  withFile p1 WriteMode (\h -> mapM_ (B.hPut h) chunks1)
  withFile p2 WriteMode (\h -> mapM_ (B.hPut h) chunks2)

showChunk1 :: ByteString -> String
showChunk1 bs =
  let [b1,b2,b3,b4,b5,b6,b7] = map fromIntegral (B.unpack bs) :: [Int] in
  unwords
    [ show (b1 `shiftL` 24 .|. b2 `shiftL` 16 .|. b3 `shiftL` 8 .|. b4)
    , showHex ((b5 .&. 0xf0) `shiftR` 4) ""
    , show (b5 .&. 0x0f)
    , show b6
    , show b7 ]

showChunk2 :: ByteString -> String
showChunk2 bs =
  let [b1,b2,b3,b4,b5,b6,b7] = map fromIntegral (B.unpack bs) :: [Int] in
  unwords
    [ show (b1 `shiftL` 24 .|. b2 `shiftL` 16 .|. b3 `shiftL` 8 .|. b4)
    , show (b5 `shiftL` 16 .|. b6 `shiftL` 8  .|. b7) ]
  

-- normalize and encode all events, then sort them by time
uncollateVoiceEvents :: MidiFile -> [B.ByteString]
uncollateVoiceEvents (MidiFile _ tracks) =
  map encodeVoice .
  map dropNumber .
  sortBy compareVoice .
  concatMap (number . undelta . voiceOnly . untrack) $ tracks

scope :: Show s => s -> s
scope x = trace (show x) x

compareVoice :: (DeltaTime, MidiVoiceEvent, Int)
             -> (DeltaTime, MidiVoiceEvent, Int)
             -> Ordering
compareVoice (a,b,c) (x,y,z) = (compare a x) <> (compare c z)

number :: [(DeltaTime, MidiVoiceEvent)] -> [(DeltaTime, MidiVoiceEvent, Int)]
number xs = zipWith (\(a,b) c -> (a,b,c)) xs [0..]

dropNumber :: (DeltaTime, MidiVoiceEvent, Int) -> (DeltaTime, MidiVoiceEvent)
dropNumber (a,b,c) = (a,b)

uncollateTempoChanges :: MidiFile -> [B.ByteString]
uncollateTempoChanges (MidiFile _ tracks) =
  map encodeTempo .
  sortBy (comparing fst) .
  concatMap (undelta . tempoOnly . untrack) $ tracks

untrack :: MidiTrack -> [(DeltaTime, MidiEvent)]
untrack (MidiTrack x) = x

-- map messages so their time is accumulated
undelta :: [(DeltaTime, a)] -> [(DeltaTime, a)]
undelta xs = undeltar xs 0 where
  undeltar [] _ = []
  undeltar ((dt, ev):xs) t = (t + dt, ev) : undeltar xs (t + dt)

voiceOnly :: [(DeltaTime, MidiEvent)] -> [(DeltaTime, MidiVoiceEvent)]
voiceOnly = catMaybes . map (\(t,e) -> case e of
  VoiceEvent _ x -> Just (t, x)
  _ -> Nothing)

tempoOnly :: [(DeltaTime, MidiEvent)] -> [(DeltaTime, Word32)]
tempoOnly = catMaybes . map (\(t,e) -> case e of
  MetaEvent (SetTempo w) -> Just (t, w)
  _ -> Nothing)

-- turn a message into 4+3 bytes
encodeVoice :: (DeltaTime, MidiVoiceEvent) -> B.ByteString
encodeVoice (t, ev) = encode32 (fromIntegral t) <> encodeVoiceEvent ev
  
-- write an encoder for the timestamp.
-- write an encoder for a voice message.
-- write an encoder for a set tempo event
-- write 

encode32 :: Word32 -> B.ByteString
encode32 w = (B.pack . map fromIntegral)
  [ (w .&. 0xff000000) `shiftR` 24
  , (w .&. 0x00ff0000) `shiftR` 16
  , (w .&. 0x0000ff00) `shiftR` 8
  ,  w .&. 0x000000ff ]

tr :: (Show a, Integral a) => a -> a
tr x = trace (showHex x "") x

encodeVoiceEvent :: MidiVoiceEvent -> B.ByteString
encodeVoiceEvent e = case e of
  NoteOff a b c        -> B.pack [0x80 .|. a, b, c]
  NoteOn a b c         -> B.pack [0x90 .|. a, b, c]
  NoteAftertouch a b c -> B.pack [0xA0 .|. a, b, c]
  Controller a b c     -> B.pack [0xB0 .|. a, b, c]
  ProgramChange a b    -> B.pack [0xC0 .|. a, b, 0]
  ChanAftertouch a b   -> B.pack [0xD0 .|. a, b, 0]
  PitchBend a bb       -> B.pack
    [ 0xE0 .|. a
    , fromIntegral ((bb .&. 0xff00) `shiftR` 8)
    , fromIntegral (bb .&. 0xff) ]

encodeTempo :: (DeltaTime, Word32) -> B.ByteString
encodeTempo (t, w) = encode32 (fromIntegral t) <> encodeTempoEvent w

encodeTempoEvent :: Word32 -> B.ByteString
encodeTempoEvent w = (B.pack . map fromIntegral)
  [ (w .&. 0xff0000) `shiftR` 16
  , (w .&. 0x00ff00) `shiftR` 8
  ,  w .&. 0x0000ff ]


  
main = do
  hPutStrLn stderr "CORE Hello World"
  Right smf <- fmap canonical <$> readMidi "midis/windfis2.mid"
  print (mf_header smf)
--  putAscii smf
  dumpMidiFile "1234" "1234" smf
--  x <- getLine
--  hPputStrLn stderr ("COR i heard " ++ x)
  threadDelay (5 * 10^6)
  hPutStrLn stderr "CORE end of"
