module Sound where

import System.IO
import System.Process
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM
import Data.Char
import Control.Monad

data PlayerCommand =
  Load String String |
  Play |
  Stop |
  Seek Int Int Int |
  SetLoop Int Int |
  EnableLoop |
  DisableLoop |
  TicksPerBeat Int |
  EnableCapture |
  DisableCapture |
  Execute Int Int Int Int |
  CutAll |
  Exit |
  Crash
    deriving (Eq, Show)

data Blub = Blub deriving (Show)
  
newSoundController :: IO (PlayerCommand -> IO (), IO Double, IO [Blub])
newSoundController = do
  (Just out, Just inn, Nothing, _) <- createProcess (proc "./sound" [])
    { std_in  = CreatePipe
    , std_out = CreatePipe }
  lock <- newMVar ()
  return
    ( \cmd -> withMVar lock $ \_ -> do
        hPutStrLn out (encodeCommand cmd)
        hFlush out
    , withMVar lock $ \_ -> do
        hPutStrLn out "tell"
        hFlush out
        line <- hGetLine inn
        case reads line of
          [(beat, "")] -> return beat
          _ -> do
            hPutStrLn stderr (concat ["CORE bad tell response (", line, ")"])
            exitFailure
    , return [Blub, Blub, Blub] )

encodeCommand :: PlayerCommand -> String
encodeCommand c = case c of
  Load p1 p2 -> unwords ["load", p1, p2]
  Play -> "play"
  Stop -> "stop"
  Seek whole num denom ->
    unwords ["seek", show whole, show num ++ "/" ++ show denom]
  SetLoop l0 l1 -> unwords ["set-loop", show l0, show l1]
  EnableLoop -> "enable-loop"
  DisableLoop -> "disable-loop"
  TicksPerBeat n -> unwords ["ticks-per-beat", show n]
  EnableCapture -> "enable-capture"
  DisableCapture -> "disable-capture"
  Execute ty ch arg1 arg2 ->
    unwords ["execute", show ty, show ch, show arg1, show arg2]
  CutAll -> "cut-all"
  Exit -> "exit"
  Crash -> "crash"

newPlayer :: (PlayerCommand -> IO ()) -> IO (Int -> IO ())
newPlayer dispatch = return $ \note -> do
  forkIO $ do
    dispatch (Execute 9 0 note 127)
    threadDelay 1000000
    --dispatch (Execute 8 0 note 127)
  return ()

