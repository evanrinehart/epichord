module Tracker where

import Control.Concurrent
import Data.IORef
import Control.Monad

import XVar
import Chart
import R2
import Mouse


type Act a = a -> IO ()
type OnClick = Act ()
type OnRelease = Act ()
type OnMove = Act ()

type TargetedMouse a = (a, Mouse)

mouseTracker :: (Eq a, Show a)
             => a -- background value
             -> IORef (R2, Chart R2 a, a, Bool, a)
             -> XVar (TargetedMouse a) -- output
             -> (Act (Chart R2 a), Act R2, Act (), Act ()) -- inputs
mouseTracker bg state out = (xl, xp, xc, xr) where
  report target target' down = do
    writeXVar out (target,  if down then LeaveDown else Leave)
    writeXVar out (target', if down then EnterDown else Enter)
  xl ch = do
    (xy, _, target, down, spooky) <- readIORef state
    let target' = atDefault l xy bg
    when (target /= target') (report target target' down)
    writeIORef state (xy, ch, target', down, spooky)
  xp xy' = do
    (xy, l, target, down, spooky) <- readIORef state
    let target' = atDefault l xy' bg
    when (target /= target') (report target target' down)
    writeIORef state (xy', l, target', down, spooky)
  xc _ = do
    (xy, l, target, _, _) <- readIORef state
    writeXVar out (target, ClickIn)
    writeIORef state (xy, l, target, True, target)
  xr _ = do
    (xy, l, target, _, spooky) <- readIORef state
    writeXVar out (target, ReleaseIn)
    case spooky of
      Just who -> writeXVar out (who, ReleaseOut)
      Nothing  -> return ()
    writeIORef state (xy, l, target, False, Nothing)
