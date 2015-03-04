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

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  let paintOut = newPaintOut paintOutH
  let rpb = repaintButton paintOut
  transfer <- newXVar (\(Transfer a b) -> if a < 1 then rpb Down else rpb Up)
  (_, position) <- newMouseTracker (0,0) chart transfer
  forkIO (keepAlive paintOut)
  handleEvents eventInH $ \i -> do
    case i of
      Mouse x y -> writeXVar position (x,y)
      KeyDown k -> print k
      Quit -> exitSuccess
      Resize w h -> print (Resize w h)
      e -> print e

keepAlive :: ([Paint] -> IO ()) -> IO ()
keepAlive paint = forever $ do
  threadDelay 10000000
  paint []

repaintButton :: ([Paint] -> IO ()) -> UpDown -> IO ()
repaintButton paint ud = case ud of
  Up   -> paint [Fill (Rect (floor 150-50) (floor 150-50) 100 100) (0,255,0)]
  Down -> paint [Fill (Rect (floor 150-50) (floor 150-50) 100 100) (255,0,0)]
