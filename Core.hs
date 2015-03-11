module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Concurrent
import System.IO
import System.Exit

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
  runProgram $ do
    (onBoot, boot)    <- newE 
    (mouse, setMouse) <- newX (0,0)
    (onClick, click)  <- newE :: Setup (E MouseButton, MouseButton -> IO ())
    (window, resize)  <- newX dim0
    (onQuit, quit)    <- newE :: Setup (E (), () -> IO ())
    input (inputWorker eventInH setMouse click resize quit)
    let (picture, sound) = program mouse onClick window onBoot
    output sound play
    output picture paint
    return (boot (), onQuit)

program :: X R2
        -> E MouseButton
        -> X R2
        -> E ()
        -> (E [Paint], E Note)
program mouse click window boot = (picture, sound) where
  sound = note <$> select              :: E Note
  select = snapshot_ click hover       :: E Int
  hover = liftA2 overQuad mouse chart  :: X Int
  chart = fmap fromLayout rects        :: X (Chart R2 Int)
  rects = liftA2 layout zero window    :: X [(Rect Double, Int)]
  picture = view simon simonView       :: E [Paint]
  simon = liftA2 Simon selection rects :: X Simon
  selection = accumulate select Nothing (\n _ -> Just n) :: X (Maybe Int)
  view = repainter boot
  zero = pure (0,0) :: X (Double, Double)

type Note = Int

note :: Int -> Note
note 0 = 62
note 1 = 63
note 2 = 67
note 3 = 70
note _ = 64

repainter :: Eq a => E () -> X a -> (a -> [Paint]) -> E [Paint]
repainter repaint x look = look <$> (edge x diff <> snapshot_ repaint x)
