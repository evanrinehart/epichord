module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Concurrent
import System.IO

import Demo
import X
import R2
import Paint
import Input
import Util
import Config
import Input
import Sound
import Chart
import Rect

main :: IO ()
main = do
  (paintOutH, eventInH, dim0) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  paint <- newPaintWorker paintOutH
  (soundA, soundB, soundC) <- newSoundController
  play <- newPlayer soundA
  (notifyBoot, boot) <- newE
  (mouse, click, window, quit) <- loadRaws eventInH dim0
  let outs = program mouse click window paint play boot
  runProgram (notifyBoot ()) quit outs

program :: X R2
        -> E MouseButton
        -> X R2
        -> ([Paint] -> IO ())
        -> (Int -> IO ())
        -> E ()
        -> [Output]
program mouse click window paint play boot = outs where
  outs = [sound, picture]
  sound = out select (play . note)
  select = snapshot_ click hover       :: E Int
  hover = liftA2 overQuad mouse chart  :: X Int
  chart = fmap fromLayout rects        :: X (Chart R2 Int)
  rects = liftA2 layout zero window    :: X [(Rect Double, Int)]
  picture = view simon simonView
  simon = liftA2 Simon selection rects :: X Simon
  selection = accumulate select Nothing (\n _ -> Just n) :: X (Maybe Int)
  view = repainter paint boot
  zero = pure (0,0) :: X (Double, Double)

note :: Int -> Int
note 0 = 62
note 1 = 63
note 2 = 67
note 3 = 70
note _ = 64

repainter :: Eq a => ([Paint] -> IO ()) -> E () -> X a -> (a -> [Paint]) -> Output
repainter paint repaint x look = 
  out (edge x diff <> snapshot_ repaint x) (paint . look)
