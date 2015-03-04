module Main where

import System.IO
import Control.Exception
import Control.Monad
import System.Exit

import Input
import Config
import Paint
import XVar

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  let paintOut = newPaintOut paintOutH
  mouse <- newXVar $ \(x,y) ->
    paintOut [Fill (Rect (floor x-50) (floor y-50) 100 100) (0,255,0)]
  handleEvents eventInH $ \i -> do
    case i of
      Mouse x y -> writeXVar mouse (x,y)
      KeyDown k -> print k
      Quit -> exitSuccess
      _ -> return ()
