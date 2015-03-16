{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Data.Monoid
import Data.List

import Debug.Trace

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

import Invader

main :: IO ()
main = do
  (paintOutH, eventInH, window0) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  paint <- newPaintWorker paintOutH
  (soundA, _, _) <- newSoundController
  play <- newPlayer soundA
  runProgram $ \onBoot time -> do
    (mouse, setMouse) <- newX (0,0)
    (onClick, click)  <- newE
    (onRelease, release)  <- newE
    (onWheel, wheel)  <- newE
    (window, resize)  <- newX window0
    (onQuit, quit)    <- newE
    (onKeydown, keydown) <- newE
    (onKeyup, keyup) <- newE
    input (inputWorker eventInH setMouse click release resize wheel quit keydown keyup)
    let (picture, sound, debug) =
           program mouse onClick onRelease window onWheel onBoot time onKeydown onKeyup
    output (const play) sound
    output (const paint) picture
    --output (const play) never
    --output (const paint) never
    output (const putStrLn) debug
    return onQuit
  putStrLn "CORE terminating"

butt :: MouseButton -> Maybe ()
butt (MouseButton 0) = Just ()
butt _ = Nothing

program :: X R2
        -> E MouseButton
        -> E MouseButton
        -> X (Rect ())
        -> E R
        -> E ()
        -> X Time
        -> E Key
        -> E Key
        -> (E [Paint], E (Either Note Note), E String)
program mouse click release window wheel boot time keydown keyup = (picture, sound, debug) where
  (frame1, frame2) = splitFrameD window (pure 0)
  (frame3, frame4) = splitFrameL frame1 (pure 60)
  wave t = if t < 0 then 60 else 30*sin t + 60
  scroll = boundedScroll wheel (pure 0.05) 0.5
  piano = pianoKeys <$> frame3 <*> scroll <*> allNotes
  chart = pianoChart <$> piano
  hover = (at' <$> chart <*> mouse)
  hoverNote = fmap pkNote <$> hover
  hoverNoteChanges = justE $ edge diff hoverNote
  buttonDown = accum
    False
    (const . either (const True) (const False))
    (click -|- release)
  combo = snapshot hoverNoteChanges buttonDown
  dragNote = fst <$> filterE snd combo
  sound0 = (Right <$> mousePlay <> keyPlay) <> (Left <$> keyStop)
  sound = sound0 <> delayE 1 sound0
  keyPlay = fromKeyboard <$> keydown
  keyStop = fromKeyboard <$> keyup
  mousePlay = clickOnKey <> dragNote
  clickOnKey = pkNote <$> justE (snapshot_ click hover)
  pianoChanged = voidE (edge diff piano)
  windowChanged = voidE (edge diff window)
  keyboardNotes = accum [] (\e ns -> case e of
    Left n -> delete n ns
    Right n -> nub (n:ns) ) (keyStop -|- keyPlay) 
  mouseAction =
    (ClickOnKey <$> clickOnKey) <>
    (DragOnKey <$> dragNote) <>
    (MouseRelease <$ release)
  mouseNotes = accum [] (\e _ -> case e of
    ClickOnKey note -> [note]
    DragOnKey note -> [note]
    MouseRelease -> [] ) mouseAction
  allNotes = (++) <$> keyboardNotes <*> mouseNotes
  repaint1 = snapshot_ (boot <> pianoChanged) (pianoView <$> piano)
  repaint2 = snapshot_ (boot <> windowChanged)
    ((\fr -> [Clip fr, Fill fr (20,20,20)]) <$> frame2)
  repaint3 = snapshot_ (boot <> windowChanged)
    ((\fr -> [Clip fr, Fill fr (15,15,15)]) <$> frame4)
  repaint4 = snapshot_ (boot <> windowChanged) mainFrame
  timeStep = never
  --pulse = unionE boot (delayE 0.1 pulse)
  --counter = accum 0 (+) (1 <$ pulse)
  --debug = show <$> snapshot_ pulse signal
  debug = show <$> edge diff time
  barn = theBarn time (voidE click)
  --barn = pure []
  xys = fff <$> barn <*> time
  fff xs t = map (\x -> (fl $ 60 + x, fl $ 100*sin t + 200)) xs
  ivs = concatMap (\xy -> invaderView xy 3 (0,255,0)) <$> xys
  mainFrame =
    (\fr ivs' -> [Clip fr, Fill fr (15,15,15)] ++ ivs') <$>
    frame4 <*> 
    ivs
  picture = repaint1 <> repaint2 <> repaint3 <> repaint4

parity :: R -> R -> Maybe Bool
parity x y | x <= 0 && y > 0 = Just False
           | x > 0 && y <= 0 = Just True
           | otherwise = Nothing

 -- barnPoke = mobDied <> mobSpawned <> timeStep
 -- invaderBarn = accum (I.empty,0) barnTrans barnPoke
 -- inv =

fl :: Double -> Double
fl = toEnum . floor

data MouseAction =
  ClickOnKey Int |
  DragOnKey Int |
  MouseRelease

theBarn :: X Time -> E () -> X [R]
theBarn time spawn = out where
  out = f <$> a <*> time 
  a = trap [] c
  c = snapshot_ spawn e
  e = g <$> a <*> time
  -- f calculates the current positions from the initial states
  f st0 t = map (h t) st0
  -- g adds a new object to the set
  g st0 t = (0, t) : st0
  h t (x0, t0) = 50*(t - t0) + x0

--barnTrans :: Either Int (R2,Color) -> (IntMap Int
