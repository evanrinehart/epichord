module Tracker where

import Control.Concurrent
import Data.IORef
import Control.Monad
import XVar

-- ok. make a square on the screen which is green normally but changes
-- to red when the mouse is over it.

type R2 = (Double, Double)
type Chart a = R2 -> a
data Transfer a = Transfer
  { leaving  :: a
  , entering :: a }
    deriving Show

chart :: Chart Int
chart (x,y) = if x < 100 || x > 200 || y < 100 || y > 200
  then 0
  else 1

newMouseTracker :: (Eq a, Show a)
                => R2 -- initial position
                -> Chart a -- initial layout
                -> XVar (Transfer a) -- output
                -> IO (XVar (Chart a), XVar R2) -- inputs
newMouseTracker xy0 l0 transfer = do
  state <- newIORef (xy0, l0, l0 xy0)
  xl <- newMVar $ \l -> do
    (xy, _, target) <- readIORef state
    let target' = l xy
    when (target /= target') (writeXVar transfer (Transfer target target'))
    writeIORef state (xy, l, target')
  xp <- newMVar $ \xy' -> do
    (xy, l, target) <- readIORef state
    putStrLn (unwords [show xy, show xy', show target])
    let target' = l xy'
    when (target /= target') (writeXVar transfer (Transfer target target'))
    writeIORef state (xy', l, target')
  return (xl, xp)
