module Main where

import System.IO
import Control.Exception
import Control.Monad
import System.Exit
import Control.Concurrent

import Control.Applicative

import Data.Monoid
import Data.Ix

import Input
import Config
import Paint
import X
--import Tracker
--import Button
import StateMachine
import Chart
import Rect
import R2

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  let painter = newPaintOut paintOutH
  forkIO (keepAlive painter)
  putStrLn "CORE Hello World"
  raws <- newRaws (640, 480) eventInH
  let mouse = rawMouse raws
  let window = rawWindowSize raws
  let quit = rawQuit raws
  sideChanged <- runDetector (lr mouse window) (/=)
  onE sideChanged print
  waitE quit

lr :: X (Double, Double) -> X (Int, Int) -> X Bool
lr mouse window = liftA2 f mouse window where
  f (x,y) (w,h) = let x' = floor x in
                  (x' < w `div` 2)

keepAlive :: ([Paint] -> IO ()) -> IO ()
keepAlive paint = forever $ do
  threadDelay 10000000
  paint []
