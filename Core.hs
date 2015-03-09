module Main where

import System.IO
import Control.Exception
import Control.Monad
import System.Exit
import Control.Concurrent
import Control.Applicative
import Data.Time
import Data.Monoid
import Data.Ix

import App
import Util
import Demo
import Paint
import X
import Chart
import Rect
import R2

main = setupWith $ \mouse window click newRepainter -> do
  putStrLn "CORE Hello World"
  let rects = layout <$> pure (0,0) <*> window
  let chart = fromLayout <$> rects
  let hover = overQuad <$> mouse <*> chart
  let select = snapshot_ click hover
  selection <- newTrap (Just <$> select) Nothing
  let simon = Simon <$> selection <*> rects
  newRepainter simon simonView
