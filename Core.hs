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
import Data.List

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
import Keys

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
    (onKeydown, keydown) <- newE
    (onKeyup, keyup) <- newE
    input (inputWorker eventInH setMouse click release resize wheel quit keydown keyup)
    input (forever (threadDelay 1000000 >> getCurrentTime >>= tick))
    let (picture, sound) =
           program mouse onClick onRelease window onWheel onBoot time onKeydown onKeyup
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
        -> E Key
        -> E Key
        -> (E [Paint], E (Either Note Note))
program mouse click release window wheel boot time keydown keyup = (picture, sound) where
  (frame1, frame2) = splitFrameD window (pure 40)
  (frame3, frame4) = splitFrameL frame1 (pure 60)
  scroll = boundedScroll wheel (pure 0.05) 0.5
  piano = pianoKeys <$> frame3 <*> scroll <*> allNotes :: X PianoKeys
  chart = pianoChart <$> piano :: X (Chart R2 PianoKey)
  hover = at' <$> chart <*> mouse :: X (Maybe PianoKey)
  --dragNote happens when the hovered note changes
  --and when the hovered note exists
  --and when the mouse button is down
  hoverNote = fmap pkNote <$> hover
  hoverNoteChanges = justE $ edge hoverNote diff
  buttonDown = accumulate (click <||> release) False
    (const . either (const True) (const False))
  combo = snapshot hoverNoteChanges buttonDown
  dragNote = fst <$> filterE snd combo
  sound = (Right <$> mousePlay <> keyPlay) <> (Left <$> keyStop)
  keyPlay = fromKeyboard <$> keydown
  keyStop = fromKeyboard <$> keyup
  mousePlay = clickOnKey <> dragNote
  clickOnKey = pkNote <$> justE (snapshot_ click hover)
  pianoChanged = () <$ edge piano diff
  windowChanged = () <$ edge window diff
  keyboardNotes = accumulate (keyStop <||> keyPlay) [] $ \e ns -> case e of
    Left n -> delete n ns
    Right n -> n:ns
  mouseAction =
    (ClickOnKey <$> clickOnKey) <>
    (DragOnKey <$> dragNote) <>
    (MouseRelease <$ release)
  mouseNotes = accumulate mouseAction [] $ \e ns -> case e of
    ClickOnKey note -> [note]
    DragOnKey note -> [note]
    MouseRelease -> []
  allNotes = (++) <$> keyboardNotes <*> mouseNotes
  repaint1 = snapshot_ (boot <> pianoChanged) (pianoView <$> piano)
  repaint2 = snapshot_ (boot <> windowChanged)
    ((\fr -> [Clip fr, Fill fr (20,20,20)]) <$> frame2)
  repaint3 = snapshot_ (boot <> windowChanged)
    ((\fr -> [Clip fr, Fill fr (15,15,15)]) <$> frame4)
  picture = repaint1 <> repaint2 <> repaint3

data MouseAction =
  ClickOnKey Int |
  DragOnKey Int |
  MouseRelease
