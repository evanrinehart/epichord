module Tracker where

import Control.Concurrent
import Data.IORef
import Control.Monad

import XVar
import Chart

data Transfer a = Transfer
  { leaving  :: a
  , entering :: a }
    deriving Show

newMouseTracker :: (Eq a, Show a)
                => R2 -- initial position
                -> Chart a -- initial layout
                -> a -- background value
                -> XVar (Transfer a) -- output
                -> IO (XVar (Chart a), XVar R2) -- inputs
newMouseTracker xy0 l0 bg transfer = do
  state <- newIORef (xy0, l0, atDefault l0 xy0 bg)
  xl <- newXVar $ \l -> do
    (xy, _, target) <- readIORef state
    let target' = atDefault l xy bg
    when (target /= target') (writeXVar transfer (Transfer target target'))
    writeIORef state (xy, l, target')
  xp <- newXVar $ \xy' -> do
    (xy, l, target) <- readIORef state
    --putStrLn (unwords [show xy, show xy', show target])
    let target' = atDefault l xy' bg
    when (target /= target') (writeXVar transfer (Transfer target target'))
    writeIORef state (xy', l, target')
  return (xl, xp)
