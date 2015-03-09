{-# LANGUAGE RankNTypes #-}
module App where

import Control.Concurrent
import Control.Monad

import R2
import Config
import Util
import X
import Paint
import Input
import Sound

type NewRepainter = forall a . Eq a => X a -> (a -> [Paint]) -> IO ()
type Setup = X R2 -> E MouseButton -> X R2
           -> NewRepainter -> (Int -> IO ()) -> IO ()

setupWith :: Setup -> IO ()
setupWith setup = do
  (paintOutH, eventInH, dim0) <- parseCommandLineOptions
  paint <- newPaintWorker paintOutH
  (soundA, soundB, soundC) <- newSoundController
  pl <- play soundA
  (raws, setSize) <- newRaws eventInH
  let mouse = rawMouse raws
  let window = rawWindowSize raws
  let quit = rawQuit raws
  let click = rawClick raws
  setup mouse click window (newRepainter paint) pl
  setSize dim0
  waitE quit

play :: (PlayerCommand -> IO ()) -> IO (Int -> IO ())
play dispatch = return $ \note -> do
  forkIO $ do
    dispatch (Execute 9 0 note 127)
    threadDelay 1000000
    --dispatch (Execute 8 0 note 127)
  return ()
  

newRepainter :: Eq a => Painter -> X a -> (a -> [Paint]) -> IO ()
newRepainter paint state look = newEdgeHandler state diff (paint . look)
