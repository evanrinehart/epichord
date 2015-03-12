module Demo where

import Data.Monoid

import R2
import Util
import Paint
import Chart
import Rect

-- the model is a bunch of numbered rectangles
layout :: Rect () -> [Rect Int]
layout area@(Rect _ l t r b) = [r0, r1, r2, r3] where
  (mx, my) = midpoint area
  r0 = Rect 0 l t mx my
  r1 = Rect 1 mx t r my
  r2 = Rect 2 l my mx b
  r3 = Rect 3 mx my r b

-- the chart is a function of the model
fromLayout :: [Rect Int] -> Chart R2 Int
fromLayout = mconcat . map rect

-- the hovered number is a function of the mouse and chart
overQuad :: R2 -> Chart R2 Int -> Int
overQuad xy ch = at ch xy 0

-- the view is a function of the widgets state
-- we dont use a signal of [Paint] because the image data might be too large
-- to detect changes in
simonView :: Simon -> [Paint]
simonView (Simon sel rects) = map f rects where
  f r = Fill (void r) (g (value r) sel)
  g 0 (Just 0) = (255, 0, 0)
  g 1 (Just 1) = (255, 255, 0)
  g 2 (Just 2) = (0, 0, 255)
  g 3 (Just 3) = (0, 255, 0)
  g 0 _ = (128, 0, 0)
  g 1 _ = (128, 128, 0)
  g 2 _ = (0, 0, 128)
  g 3 _ = (0, 128, 0)

-- a widgets state
data Simon = Simon
  { selection :: Maybe Int
  , rects :: [Rect Int] }
    deriving (Show, Eq)

