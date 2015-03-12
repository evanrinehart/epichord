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

import Tools

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
    (onWheel, wheel)  <- newE
    (window, resize)  <- newX window0
    (onQuit, quit)    <- newE
    (time, tick)      <- newE
    input (inputWorker eventInH setMouse click release resize wheel quit)
    input (forever (threadDelay 1000000 >> getCurrentTime >>= tick))
    let (picture, sound) =
           program mouse onClick onRelease window onWheel onBoot time
    output sound play
    output picture paint
    return (boot (), onQuit)

program :: X R2
        -> E MouseButton
        -> E MouseButton
        -> X (Rect ())
        -> E Double
        -> E ()
        -> E UTCTime
        -> (E [Paint], E Note)
program mouse click release window wheel boot time = (picture, sound) where
  (frame1, frame2) = splitFrameD window (pure 50)
  (frame3, frame4) = splitFrameL frame1 (pure 50)
  scroll = boundedScroll wheel (pure 0.05) 0.5
  piano = pianoKeys <$> frame3 <*> scroll <*> ons :: X PianoKeys
  chart = pianoChart <$> piano :: X (Chart R2 PianoKey)
  hover = at' <$> chart <*> mouse :: X (Maybe PianoKey)
  holding = accumulate (click <||> release) False $ \e _ -> case e of
    Left _ -> True
    Right _ -> False
  sound = dragOnKey
  ons = accumulate (dragOnKey <||> release) [] $ \e s -> case e of
    Left note -> [note]
    Right _ -> []
  clickOnKey = pkNote <$> justE (snapshot_ click hover)
  hoverDown = (,) <$> hover <*> holding
  hoverDownChanged = edge hoverDown diff
  --dragOnKey = justE . fmap fuck $ hoverDownChanged
  dragOnKey = justE (fuck <$> hoverDownChanged)
  pianoChanged = () <$ edge piano diff
  repaint = snapshot_ (boot <> pianoChanged) (pianoView <$> piano)
  picture = repaint

fuck :: (Maybe PianoKey, Bool) -> Maybe Int
fuck (Nothing, _) = Nothing
fuck (Just pk, False) = Nothing
fuck (Just pk, True) = Just $ pkNote pk
    
