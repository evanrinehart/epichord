{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Control.Concurrent
import System.IO
import System.Exit
import Data.Time
import Data.Maybe

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
import Piano

main :: IO ()
main = do
  (paintOutH, eventInH, window0) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  paint <- newPaintWorker paintOutH
  (soundA, soundB, soundC) <- newSoundController
  play <- newPlayer soundA
  runProgram $ do
    (onBoot, boot)    <- newE 
    (mouse, setMouse) <- newX (0,0)
    (onClick, click)  <- newE
    (onRelease, release)  <- newE
    (window, resize)  <- newX window0
    (onQuit, quit)    <- newE
    (time, tick)      <- newE
    input (inputWorker eventInH setMouse click release resize quit)
    input (forever (threadDelay 1000000 >> getCurrentTime >>= tick))
    let (picture, sound, printer) =
           program mouse onClick onRelease window onBoot time
    output sound play
    --output sound print
    --output time print
    output picture paint
    --output printer print
    return (boot (), onQuit)

mkay :: Either UTCTime Int -> Int -> Int
mkay (Left _) n = (n+1) `mod` 4
mkay (Right m) _ = m

program :: X R2
        -> E MouseButton
        -> E MouseButton
        -> X (Rect ())
        -> E ()
        -> E UTCTime
        -> (E [Paint], E Note, E (Maybe PianoKey))
program mouse click release window boot time = (picture, sound, printer) where
  piano = pianoKeys <$> window <*> (pure 0) <*> ons :: X PianoKeys
  chart = pianoChart <$> piano :: X (Chart R2 PianoKey)
  hover = at' <$> chart <*> mouse :: X (Maybe PianoKey)
  --picture = view piano (const (pure []))
  repaint = snapshot_ (boot <> pianoChanged) (pianoView <$> piano)
  down = pkNote <$> justE (snapshot_ click hover)
  up = release
  ons = accumulate (up <||> down) [] $ \e s -> case e of
    Left _ -> []
    Right note -> note : s
  pianoChanged = () <$ edge piano diff
  picture = repaint
  sound = pkNote <$> justE (snapshot_ click hover)
  printer = edge hover diff
  --simon = liftA2 Simon selection rects :: X Simon
  --selection = accumulate select Nothing (\n _ -> Just n) :: X (Maybe Int)
  --selection = Just <$> timeN
  --view = repainter boot

note :: Int -> Note
note 0 = 62
note 1 = 63
note 2 = 67
note 3 = 70
note _ = 64

repainter :: Eq a => E () -> X a -> (a -> [Paint]) -> E [Paint]
repainter repaint x look = look <$> (edge x diff <> snapshot_ repaint x)
