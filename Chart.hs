module Chart where

import Data.Maybe
import Data.Monoid
import Data.Profunctor
import Control.Applicative

import Rect
import R2

newtype Chart a b = Chart { unchart :: a -> Maybe b }

instance Functor (Chart a) where
  fmap = rmap

instance Profunctor Chart where
  -- lmap :: (b -> a) -> Chart a z -> Chart b z
  lmap f (Chart g) = Chart (g . f)
  rmap f (Chart g) = Chart (fmap f . g)

instance Applicative (Chart a) where
  pure x = Chart (\_ -> Just x)
  ff <*> xx = Chart $ \coord -> case (unchart ff coord, unchart xx coord) of
    (Just f, Just x) -> Just (f x)
    _ -> Nothing

instance Monoid (Chart a b) where
  mempty = Chart (\_ -> Nothing)
  mappend (Chart f) (Chart g) = Chart h where
    h xy = getFirst (First (f xy) <> First (g xy))

at :: Chart a b -> a -> b -> b
at ch xy d = fromMaybe d (unchart ch xy)

at' :: Chart a b -> a -> Maybe b
at' = unchart

rect :: Rect a -> Chart R2 a
rect r = clip r (pure (value r))

clip :: Rect a -> Chart R2 b -> Chart R2 b
clip r ch = Chart $ \(x,y) ->
  if outsideRect x y r
    then Nothing
    else unchart ch (x,y)
