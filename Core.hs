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
import Chart
import Rect

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  let paint = newPaintOut paintOutH
  let newButton = makeNewButton paint
  button <- newButton buttonLook (putStrLn "BOOYA")
  character <- newXVar (\c -> return ())
  windowSize <- newXVar (\(w,h) -> return ())
  transfer <- newXVar $ \t -> case t of
    Transfer 1 _ -> writeXVar button Out
    Transfer _ 1 -> writeXVar button In
  (_, position) <- newMouseTracker (0,0)
    (rect (Rect 0 0 100 100) 1) 0 transfer
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

buttonLook :: UpDown -> [Paint]
buttonLook ud = case ud of
  Up   -> [Fill (Rect 0 0 100 100) (0,255,0)]
  Down -> [Fill (Rect 0 0 100 100) (0,0,255)]

makeNewButton :: Painter
              -> (UpDown -> [Paint])
              -> IO ()
              -> IO (XVar MouseActivity)
makeNewButton paint look go =
  newStateMachine (buttonSM (paint . look) go) OutUp
