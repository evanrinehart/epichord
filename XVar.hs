module XVar where

import Control.Concurrent
import Data.Functor

newtype XVar a = XVar (MVar (a -> IO ()))

newXVar :: (a -> IO ()) -> IO (XVar a)
newXVar act = XVar <$> newMVar act

writeXVar :: XVar a -> a -> IO ()
writeXVar (XVar mv) x = do
  forkIO $ withMVar mv (\act -> act x)
  return ()

changeXVarEffect :: XVar a -> (a -> IO ()) -> IO ()
changeXVarEffect (XVar mv) act = do
  forkIO $ modifyMVar_ mv (\_ -> return act)
  return ()
