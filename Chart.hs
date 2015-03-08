module Chart where

import Data.Maybe
import Data.Monoid

import Rect
import R2

newtype Chart a b = Chart { unchart :: a -> Maybe b }

instance Monoid (Chart a b) where
  mempty = Chart (\_ -> Nothing)
  mappend (Chart f) (Chart g) = Chart h where
    h xy = case g xy of
      Nothing -> f xy
      Just z  -> Just z

instance Functor (Chart a) where
  fmap f (Chart g) = Chart (fmap f . g)

at :: Chart a b -> a -> Maybe b
at = unchart

atDefault :: Chart a b -> a -> b -> b
atDefault ch xy d = fromMaybe d (unchart ch xy)

rect :: (Num a, Ord a) => Rect a -> b -> Chart (a,a) b
rect r v = Chart (\xy -> if inRect r xy then Just v else Nothing)
