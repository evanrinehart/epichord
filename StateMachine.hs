module StateMachine where

import XVar

type StateMachine i a :: (i -> a -> (a, IO ()))

newStateMachine :: StateMachine i a -> a -> IO (XVar i)
newStateMachine sm st = do
  ref <- newIORef st
  newMVar $ \i -> do
    s <- readIORef ref
    let (s', act) = sm i s
    act s'
    writeIORef ref s'
