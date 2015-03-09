module Main where

import Control.Applicative

import App
import X
import Demo

main :: IO ()
main = setupWith $ \mouse click window newRepainter -> do
  putStrLn "CORE Hello World"
  let rects = liftA2 layout (pure (0,0)) window
  let chart = fromLayout <$> rects
  let hover = liftA2 overQuad mouse chart
  let select = snapshot_ click hover
  selection <- newTrap (Just <$> select) Nothing
  let simon = liftA2 Simon selection rects
  newRepainter simon simonView
