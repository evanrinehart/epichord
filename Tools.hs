module Tools where

import Control.Applicative

import Control.Broccoli

import Rect

boundedScroll :: E Double -> X Double -> Double -> X Double
boundedScroll go scale start =
  accumulate (snapshot go scale) start boundedScrollTrans

boundedScrollTrans :: (Double, Double) -> Double -> Double
boundedScrollTrans (delta, scale) s =
  if delta > 0
    then min 1 (s+delta*scale)
    else max 0 (s+delta*scale)

splitFrameL :: X Frame -> X Double -> (X Frame, X Frame)
splitFrameL parent division = (frL, frR) where
  frL = f1 <$> parent <*> division
  frR = f2 <$> parent <*> division
  f1 (Rect _ l t r b) d = Rect () l     t (l+d) b
  f2 (Rect _ l t r b) d = Rect () (l+d) t     r b

splitFrameD :: X Frame -> X Double -> (X Frame, X Frame)
splitFrameD parent division = (frU, frD) where
  frU = f1 <$> parent <*> division
  frD = f2 <$> parent <*> division
  f1 (Rect _ l t r b) d = Rect () l     t r (b-d)
  f2 (Rect _ l t r b) d = Rect () l (b-d) r     b
