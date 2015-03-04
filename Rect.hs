module Rect where

import R2

data Rect a = Rect
  { top :: a
  , left :: a
  , width :: a
  , height :: a }
    deriving Show

inRect :: (Num a, Ord a) => Rect a -> (a,a) -> Bool
inRect r xy = not (outsideRect r xy)

outsideRect :: (Num a, Ord a) => Rect a -> (a,a) -> Bool
outsideRect (Rect x y w h) (x',y') =
  x' < x ||
  x' > x+w ||
  y' < y ||
  y' > y+h

translateRect :: Num a => Rect a -> (a,a) -> Rect a
translateRect (Rect x y w h) (dx, dy) = Rect (x + dx) (y + dy) w h

