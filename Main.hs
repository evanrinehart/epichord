{-# LANGUAGE TupleSections #-}
module Main where

import Control.Applicative
import Data.Monoid
import Data.List
import System.Exit

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
  getIn <- newInputWorker eventInH
  simulate (program (0,0) window0) getIn $ \t o -> case o of
    Sound e -> play e
    PaintStuff ps -> paint ps
    Shutdown -> do
      putStrLn "CORE terminating"
      exitSuccess

data Out =
  Sound (Either Note Note) |
  PaintStuff [Paint] |
  Shutdown

program :: R2 -> Rect () -> E RawInput -> E Out
program xy0 wh0 input = output where
  -- inputs
  mouse = trap xy0 (maybeE mouseOnly input)
  window = trap wh0 (maybeE resizeOnly input)
  click = maybeE clickOnly input
  release = maybeE releaseOnly input
  wheel = maybeE wheelOnly input
  keydown = maybeE keydownOnly input
  keyup = maybeE keyupOnly input
  quit = maybeE quitOnly input
  -- window frames
  (frame1, frame2) = splitFrameD window (pure 0)
  (frame3, frame4) = splitFrameL frame1 (pure 60)
  -- outputs
  picture =
    (pianoView <$> piano) <>
    (darkGray <$> frame4)
  sound =
    (Right <$> keyPlay) <>
    (Left <$> keyStop) <>
    (Right <$> mouseOnKey)
  output =
    (PaintStuff <$> bootEdge picture) <>
    (Shutdown <$ quit) <>
    (Sound <$> sound)
  -- models
  piano = pianoKeys <$> frame3 <*> scroll <*> allNotes
  allNotes = liftA2 (++) keyboardNotes mouseNotes
  keyboardNotes = simpleBarn keyStop keyPlay
  mouseNotes = singleBarn release mouseOnKey
  chart = pianoChart <$> piano
  hover = at' <$> chart <*> mouse
  scroll = boundedScroll wheel (pure 0.05) 0.5
  button = buttonDown click release
  -- controls
  keyPlay = fromKeyboard <$> keydown
  keyStop = fromKeyboard <$> keyup
  clickOnKey = pkNote <$> justE (snapshot_ hover click)
  dragOnKey = pkNote <$> whenE button (dragChanged chart2 mouse)
  mouseOnKey = clickOnKey <> dragOnKey
  -- misc
  --wave = fmap (\t -> 60*sin t + 100) time
  chart2 = pianoChart <$> piano2
  piano2 = pianoKeys <$> frame3 <*> scroll <*> pure []

simpleBarn :: Eq a => E a -> E a -> X [a]
simpleBarn eDel eAdd = mealy [] f (eitherE eDel eAdd) where
  f (Left n) ns = delete n ns
  f (Right n) ns = nub (n:ns)

singleBarn :: Eq a => E a -> E b -> X [b]
singleBarn eDel eAdd = mealy [] f (eitherE eDel eAdd) where
  f (Left _) _ = []
  f (Right n) _ = [n]

bootEdge :: X a -> E a
bootEdge x = snapshot_ x boot <> (snd <$> edge x)

darkGray :: Frame -> [Paint]
darkGray fr = [Clip fr, Fill fr (15,15,15)]

rasterize :: Int -> X a -> X a
rasterize sr x = trap (atZero x) (snapshot_ x (pulse (1 / realToFrac sr)))

dragChanged :: Eq a => X (Chart R2 a) -> X R2 -> E a
dragChanged chart pos = maybeE f (edge (liftA2 at' chart pos)) where
  f (Nothing, Nothing) = Nothing
  f (n0, n1) = if n0 == n1 then Nothing else n1

buttonDown :: E a -> E b -> X Bool
buttonDown down up =
  accum False (((const True) <$ down) <> ((const False) <$ up))

{-
  wave t = if t < 0 then 60 else 30*sin t + 60
  scroll = boundedScroll wheel (pure 0.05) 0.5
  piano = pianoKeys <$> frame3 <*> scroll <*> allNotes
  chart = pianoChart <$> piano
  hover = (at' <$> chart <*> mouse)
  hoverNote = fmap pkNote <$> hover
  hoverNoteChanges = justE . justE . fmap (uncurry diff) . edge $ hoverNote
  buttonDown = mealy
    False
    (const . either (const True) (const False))
    (click `eitherE` release)
  combo :: E (Bool, Note)
  combo = snapshot (,) buttonDown hoverNoteChanges
  dragNote = snd <$> filterE fst combo
  sound0 = (Right <$> mousePlay <> keyPlay) <> (Left <$> keyStop)
  sound = sound0 <> delayE 1 sound0
  keyPlay = fromKeyboard <$> keydown
  keyStop = fromKeyboard <$> keyup
  mousePlay = clickOnKey <> dragNote
  clickOnKey = pkNote <$> justE (snapshot_ hover click)
  pianoChanged = voidE (edge piano)
  keyboardNotes = mealy [] (\e ns -> case e of
    Left n -> delete n ns
    Right n -> nub (n:ns) ) (keyStop `eitherE` keyPlay) 
  mouseAction =
    (ClickOnKey <$> clickOnKey) <>
    (DragOnKey <$> dragNote) <>
    (MouseRelease <$ release)
  mouseNotes = mealy [] (\e _ -> case e of
    ClickOnKey note -> [note]
    DragOnKey note -> [note]
    MouseRelease -> [] ) mouseAction
  allNotes = (++) <$> keyboardNotes <*> mouseNotes
  repaint1 = snapshot_ (pianoView <$> piano) (boot <> pianoChanged)
  repaint2 = snapshot_ ((\fr -> [Clip fr, Fill fr (20,20,20)]) <$> frame2)
    (boot <> windowChanged_)
  repaint3 = snapshot_ ((\fr -> [Clip fr, Fill fr (15,15,15)]) <$> frame4)
    (boot <> windowChanged_)
  repaint4 = snapshot_ mainFrame (boot <> windowChanged_)
  timeStep = never
  --counter = accum 0 (+) (1 <$ pulse)
  --debug = show <$> snapshot_ pulse signal
  --debug = show <$> edge diff time
  debug = never
  barn = theBarn time (voidE click)
  --barn = pure []
  xys = fff <$> barn <*> time
  fff xs t = map (\x -> (fl $ 60 + x, fl $ 100*sin t + 200)) xs
  iii xy = invaderView xy 3 (0,255,0)
  ivs = concatMap (\xy -> invaderView xy 3 (0,255,0)) <$> xys
  mainFrame =
    (\fr ivs' -> [Clip fr, Fill fr (15,15,15)] ++ ivs') <$>
    frame4 <*> 
    ivs
  blank fr = [Clip fr, Fill fr (15,15,15)]
  flows m = multiplex (map (\i -> timeShift (i/4) m) [0,1,2,3,4,5])
  picture = snapshot_ ((blank <$> frame4) <> (concatMap iii <$> (flows mouse))) (pulse 0.3) <> repaint1
-}

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
theBarn t spawn = out where
  out = f <$> a <*> t 
  a = trap [] c
  c = snapshot_ e spawn
  e = g <$> a <*> t
  -- f calculates the current positions from the initial states
  f st0 u = map (h u) st0
  -- g adds a new object to the set
  g st0 u = (0, u) : st0
  h u (x0, t0) = 50*(u - t0) + x0

--barnTrans :: Either Int (R2,Color) -> (IntMap Int
