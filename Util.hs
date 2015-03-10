module Util where

import R2
import Rect

floor2 :: R2 -> Z2
floor2 (x,y) = (floor x, floor y)

diff :: Eq a => a -> a -> Maybe a
diff x y = if x == y then Nothing else Just y

n2r :: N2 -> R2
n2r (x,y) = (realToFrac x, realToFrac y)

floorR :: Rect Double -> Rect Int
floorR (Rect x y w h) = Rect (floor x) (floor y) (floor w) (floor h)

circle :: Double -> Double -> (Double, Double)
circle r t = (r * cos t, r * sin t)

