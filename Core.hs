module Main where

import Control.Applicative

import App
import Demo
import X

main :: IO ()
main = setupWith $ \mouse click window newRepainter play -> do
  putStrLn "CORE Hello World"
  let rects = liftA2 layout (pure (0,0)) window
  let chart = fromLayout <$> rects
  let hover = liftA2 overQuad mouse chart
  let select = snapshot_ click hover
  selection <- newTrap (Just <$> select) Nothing
  newEventHandler select $ \n -> do
    case n of
      0 -> play 62
      1 -> play 63
      2 -> play 67
      3 -> play 70
  let simon = liftA2 Simon selection rects
  newRepainter simon simonView
