module Main where

import System.IO
import Control.Exception
import Control.Monad
import System.Exit
import Control.Concurrent

import Input
import Config
import Paint
import XVar
import Tracker
import Button
import StateMachine

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  let paintOut = newPaintOut paintOutH
  let rpb = repaintButton paintOut
  button <- newButton paintOut buttonLook (putStrLn "BOOYA")
  character <- newXVar (\c -> return ())
  windowSize <- newXVar (\(w,h) -> return ())
  transfer <- newXVar $ \t -> case t of
    Transfer 1 _ -> writeXVar button Out
    Transfer _ 1 -> writeXVar button In
  (_, position) <- newMouseTracker (0,0) chart transfer
  handleEvents eventInH $ \i -> do
    case i of
      Mouse x y -> writeXVar position (x,y)
      Click (MouseButton 0) -> writeXVar button MClick
      Release (MouseButton 0) -> writeXVar button MRelease
      KeyDown k -> print k
      Quit -> exitSuccess
      Resize w h -> writeXVar windowSize (w,h)
      Character c -> writeXVar character c
      e -> print e

keepAlive :: ([Paint] -> IO ()) -> IO ()
keepAlive paint = forever $ do
  threadDelay 10000000
  paint []

repaintButton :: ([Paint] -> IO ()) -> UpDown -> IO ()
repaintButton paint ud = paint (buttonLook ud)

buttonLook :: UpDown -> [Paint]
buttonLook ud = case ud of
  Up   -> [Fill (Rect (floor 150-50) (floor 150-50) 100 100) (0,255,0)]
  Down -> [Fill (Rect (floor 150-50) (floor 150-50) 100 100) (255,0,0)]

newButton :: ([Paint] -> IO ())
          -> (UpDown -> [Paint]) -> IO () -> IO (XVar MouseActivity)
newButton paint look go =
  newStateMachine (buttonSM (repaintButton paint) go) OutUp
