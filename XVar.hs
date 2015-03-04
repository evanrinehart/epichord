module XVar where

import Control.Concurrent

type XVar a = MVar (a -> IO ())

-- mouse :: XVar (Double, Double)
-- playPos :: XVar Double
-- click :: XVar ()
-- release :: XVar ()

-- an WVar is like an IORef that you can only write to.
-- when you write to it, it has some side effect.
-- you can reprogram the side effect.

-- because its currently an MVar, writing to it during the effect of writing
-- to it will cause a deadlock.

writeXVar :: XVar a -> a -> IO ()
writeXVar xv x = withMVar xv (\act -> act x)

changeXVarEffect :: XVar a -> (a -> IO ()) -> IO ()
changeXVarEffect xv act = modifyMVar_ xv (const act)
