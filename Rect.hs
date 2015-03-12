module Rect where

import Data.Functor

import R2

outsideRect :: R -> R -> Rect a -> Bool
outsideRect x y (Rect _ l t r b) = x < l || x > r || y < t || y > b

inRect :: R -> R -> Rect a -> Bool
inRect x y r = not (outsideRect x y r)

data Rect a = Rect
  { value :: a
  , left :: R
  , top :: R
  , right :: R
  , bottom :: R }
    deriving (Eq, Show)

instance Functor Rect where
  fmap f (Rect x l t r b) = Rect (f x) l t r b

type Frame = Rect ()

void :: Rect a -> Rect ()
void = (() <$)

unit :: Rect ()
unit = Rect () 0 0 1 1

translateRect :: R -> R -> Rect a -> Rect a
translateRect x y (Rect v l t r b) = Rect v (l+x) (t+y) (r+x) (b+y)

scaleRect :: R -> R -> Rect a -> Rect a
scaleRect u v (Rect a l t r b) = Rect a (l*u) (t*v) (r*u) (b*v)

midpoint :: Rect a -> R2
midpoint (Rect _ l t r b) = (x,y) where
  x = (r - l) / 2
  y = (b - t) / 2
