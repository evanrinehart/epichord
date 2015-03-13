{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Data.Monoid
import Data.List

import Control.Broccoli

import R2
import Paint
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
  (soundA, _, _) <- newSoundController
  play <- newPlayer soundA
  runProgram $ do
    (onBoot, boot)    <- newE 
    (mouse, setMouse) <- newX (0,0)
    (onClick, click)  <- newE
    (onRelease, release)  <- newE
    (onWheel, wheel)  <- newE
    (window, resize)  <- newX window0
    (onQuit, quit)    <- newE
    time              <- newTime
    (onKeydown, keydown) <- newE
    (onKeyup, keyup) <- newE
    input (inputWorker eventInH setMouse click release resize wheel quit keydown keyup)
    let (picture, sound, debug) =
           program mouse onClick onRelease window onWheel onBoot time onKeydown onKeyup
    output sound (const play)
    output picture (const paint)
    output debug (const print)
    return (boot (), onQuit)
  putStrLn "CORE terminating"

butt :: MouseButton -> Maybe ()
butt (MouseButton 0) = Just ()
butt _ = Nothing

program :: X R2
        -> E MouseButton
        -> E MouseButton
        -> X (Rect ())
        -> E Double
        -> E ()
        -> X Time
        -> E Key
        -> E Key
        -> (E [Paint], E (Either Note Note), E String)
program mouse click release window wheel boot time keydown keyup = (picture, sound, debug) where
  debug = show <$> snapshot click time
  (frame1, frame2) = splitFrameD window (pure 40)
  (frame3, frame4) = splitFrameL frame1 (pure 60)
  scroll = boundedScroll wheel (pure 0.05) 0.5
  piano = pianoKeys <$> frame3 <*> scroll <*> allNotes :: X PianoKeys
  chart = pianoChart <$> piano :: X (Chart R2 PianoKey)
  hover = at' <$> chart <*> mouse :: X (Maybe PianoKey)
  hoverNote = fmap pkNote <$> hover
  hoverNoteChanges = justE $ edge hoverNote diff
  buttonDown = accumulate (click -|- release) False
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
  keyboardNotes = accumulate (keyStop -|- keyPlay) [] $ \e ns -> case e of
    Left n -> delete n ns
    Right n -> n:ns
  mouseAction =
    (ClickOnKey <$> clickOnKey) <>
    (DragOnKey <$> dragNote) <>
    (MouseRelease <$ release)
  mouseNotes = accumulate mouseAction [] $ \e _ -> case e of
    ClickOnKey note -> [note]
    DragOnKey note -> [note]
    MouseRelease -> []
  allNotes = (++) <$> keyboardNotes <*> mouseNotes
  repaint1 = snapshot_ (boot <> pianoChanged) (pianoView <$> piano)
  repaint2 = snapshot_ (boot <> windowChanged)
    ((\fr -> [Clip fr, Fill fr (20,20,20)]) <$> frame2)
  repaint3 = snapshot_ (boot <> windowChanged)
    ((\fr -> [Clip fr, Fill fr (15,15,15)]) <$> frame4)
--  repaint5 = snapshot_ vsync 
  picture = repaint1 <> repaint2 <> repaint3

data MouseAction =
  ClickOnKey Int |
  DragOnKey Int |
  MouseRelease

