module Tools where

import Control.Applicative

import Control.Broccoli

import Rect
import Keys

boundedScroll :: E Double -> X Double -> Double -> X Double
boundedScroll go scale start =
  mealy start boundedScrollTrans (snapshot (,) scale go)

boundedScrollTrans :: (Double, Double) -> Double -> Double
boundedScrollTrans (scale, delta) s =
  if delta > 0
    then min 1 (s+delta*scale)
    else max 0 (s+delta*scale)

splitFrameL :: X Frame -> X Double -> (X Frame, X Frame)
splitFrameL parent division = (frL, frR) where
  frL = f1 <$> parent <*> division
  frR = f2 <$> parent <*> division
  f1 (Rect _ l t _ b) d = Rect () l     t (l+d) b
  f2 (Rect _ l t r b) d = Rect () (l+d) t     r b

splitFrameD :: X Frame -> X Double -> (X Frame, X Frame)
splitFrameD parent division = (frU, frD) where
  frU = f1 <$> parent <*> division
  frD = f2 <$> parent <*> division
  f1 (Rect _ l t r b) d = Rect () l     t r (b-d)
  f2 (Rect _ l _ r b) d = Rect () l (b-d) r     b

fromKeyboard :: Key -> Int
fromKeyboard k = case k of
  Q -> 60
  Two -> 61
  W -> 62
  Three -> 63
  E -> 64
  R -> 65
  Five -> 66
  T -> 67
  Six -> 68
  Y -> 69
  Seven -> 70
  U -> 71
  I -> 72
  Nine -> 73
  O -> 74
  Zero -> 75
  P -> 76
  M -> 59
  J -> 58
  N -> 57
  H -> 56
  B -> 54
  _ -> 60


counter :: Int -> E () -> X Int
counter start bump = out where
  out = trap 0 bumpToNumber
  bumpToNumber = snapshot_ nextNumber bump
  nextNumber = fmap (+1) out


{-
data Barn i s = NewAnimal i s | RetireAnimal i

barn :: E (Barn i s) -> X [s]
barn ii = out where
  out = trap [] out'
  out' = fmap f (snapshot ii out)
  f (i, s) = case i of
    NewAnimal i x -> (i,x) : s
    RetireAnimal i -> deleteBy ((==i) . fst) s

timeBarn :: (Time -> s -> a) -> X Time -> E (Barn i s) -> X [a]
timeBarn f time ii = fmap (\x -> f <$> time <*> x) (barn ii)
    

intBarn :: E IntBarn -> X [Int]
intBarn i = out where
  s = trap (0, []) s'
  s' = fmap f (snapshot i out)
  f (i, (n', s)) = case i of
    NewInt -> (n'+1, n':s)
    RetireInt n -> (n', delete n s)
  out = fmap snd s
-}
