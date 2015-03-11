{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Concurrent
import System.IO
import System.Exit
import Data.Time

import Demo
import Control.Broccoli
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
    (onClick, click)  <- newE
    (window, resize)  <- newX dim0
    (onQuit, quit)    <- newE
    (time, tick)      <- newE
    input (inputWorker eventInH setMouse click resize quit)
    input (forever (threadDelay 1000000 >> getCurrentTime >>= tick))
    let (picture, sound) = program mouse onClick window onBoot time
    --output sound play
    --output sound print
    --output time print
    output picture paint
    return (boot (), onQuit)

mkay :: Either UTCTime Int -> Int -> Int
mkay (Left _) n = (n+1) `mod` 4
mkay (Right m) _ = m

program :: X R2
        -> E MouseButton
        -> X R2
        -> E ()
        -> E UTCTime
        -> (E [Paint], E Note)
program mouse click window boot time = (picture, sound) where
  timeN = debugX $ accumulate (time <||> override) 0 mkay
  override = (\x -> (x+2) `mod` 4) <$> snapshot_ click timeN
  sound = note <$> select              :: E Note
  ships = accumulate sound 0 (+)
  offset = (,0) . realToFrac <$> ships
  --select = snapshot_ click hover       :: E Int
  select = never
  hover = liftA2 overQuad mouse chart  :: X Int
  chart = fmap fromLayout rects        :: X (Chart R2 Int)
  rects = liftA2 layout offset window  :: X [(Rect Double, Int)]
  picture = view simon simonView       :: E [Paint]
  simon = liftA2 Simon selection rects :: X Simon
  --selection = accumulate select Nothing (\n _ -> Just n) :: X (Maybe Int)
  selection = Just <$> timeN
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
