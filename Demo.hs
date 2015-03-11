module Demo where

import Data.Monoid

import R2
import Util
import Paint
import Chart
import Rect

-- the model is a bunch of numbered rectangles
layout :: R2 -> R2 -> [(Rect Double, Int)]
layout (x,y) (w,h) = [(r1,0), (r2,1), (r3,2), (r4,3)] where
  (w2, h2) = (w / 2, h / 2)
  r1 = Rect x y w2 h2
  r2 = Rect (x+w2) y (w2+1) h2
  r3 = Rect x (y+h2) w2 (h2+1)
  r4 = Rect (x+w2) (y+h2) (w2+1) (h2+1)

-- the chart is a function of the model
fromLayout :: [(Rect Double, Int)] -> Chart R2 Int
fromLayout rects = mconcat (map (\(r,i) -> rect r i) rects)

-- the hovered number is a function of the mouse and chart
overQuad :: R2 -> Chart R2 Int -> Int
overQuad xy ch = atDefault ch xy 0

-- the view is a function of the widgets state
-- we dont use a signal of [Paint] because the image data might be too large
-- to detect changes in
simonView :: Simon -> [Paint]
simonView (Simon sel rects) = map f rects where
  f (r,c) = Fill (floorR r) (g c sel)
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
  , rects :: [(Rect Double, Int)] }
    deriving (Show, Eq)

