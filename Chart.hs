module Chart where

import Data.Maybe
import Data.Monoid

import Rect

type R2 = (Double, Double)

newtype Chart a = Chart { unchart :: R2 -> Maybe a }

instance Monoid (Chart a) where
  mempty = Chart (\_ -> Nothing)
  mappend (Chart f) (Chart g) = Chart h where
    h xy = case g xy of
      Nothing -> f xy
      Just z  -> Just z

instance Functor Chart where
  fmap f (Chart g) = Chart (fmap f . g)

at :: Chart a -> R2 -> Maybe a
at = unchart

atDefault :: Chart a -> R2 -> a -> a
atDefault ch xy d = fromMaybe d (unchart ch xy)

rect :: Rect Double -> b -> Chart b
rect r v = Chart (\xy -> if inRect r xy then Just v else Nothing)
