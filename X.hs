{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module X where

import System.Random
import Control.Applicative
import Data.Functor
import Data.Monoid
import Control.Monad
import Data.Unamb
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Data.Function

data X a where
  PureX :: a -> X a
  FmapX :: forall a b . (b -> a) -> X b -> X a
  ApplX :: forall a b . X (b -> a) -> X b -> X a
  PortX :: TVar a -> X a

instance Functor X where
  fmap f x = FmapX f x

instance Applicative X where
  pure x = PureX x
  f <*> x = ApplX f x

data E a where
  NeverE    :: E a
  FmapE     :: forall a b . (b -> a) -> E b -> E a
  MappendE  :: E a -> E a -> E a
  ProductE  :: (b -> c -> a) -> E b -> E c -> E a
  SnapshotE :: E b -> X a -> E a
  PortE     :: TChan a -> E a
  FilterE   :: (a -> Bool) -> E a -> E a

instance Functor E where
  fmap f e = FmapE f e

instance Monoid (E a) where
  mempty = NeverE
  mappend e1 e2 = MappendE e1 e2

snapshot :: E a -> X b -> E (a,b)
snapshot e x = ProductE (,) e (SnapshotE e x)

filterE :: (a -> Bool) -> E a -> E a
filterE f e = FilterE f e

dupE :: E a -> IO (E a)
dupE e = case e of
  NeverE -> return NeverE
  FmapE f e' -> do
    e'' <- dupE e'
    return (FmapE f e'')
  MappendE e1 e2 -> do
    e1' <- dupE e1
    e2' <- dupE e2
    return (MappendE e1' e2')
  PortE ch -> do
    ch' <- atomically (dupTChan ch)
    return (PortE ch')
  ProductE f e1 e2 -> do
    e1' <- dupE e1
    e2' <- dupE e2
    return (ProductE f e1' e2')
  SnapshotE e' x -> do
    e'' <- dupE e'
    return (SnapshotE e'' x)
  FilterE f e' -> do
    e'' <- dupE e'
    return (FilterE f e'')

readE :: E a -> IO a
readE e = case e of
  NeverE -> hang
  PortE ch -> atomically (readTChan ch)
  MappendE e1 e2 -> race (readE e1) (readE e2)
  FmapE f e' -> f <$> readE e'
  ProductE f e1 e2 -> do
    x <- readE e1
    y <- readE e2
    return (f x y)
  SnapshotE e' x -> do
    readE e'
    atomically (readX x)
  FilterE f e' -> fix $ \loop -> do
    x <- readE e'
    if f x
      then return x
      else loop

runStateMachine :: E a -> s -> (a -> s -> s) -> IO (X s)
runStateMachine e0 s0 trans = do
  ref <- newIORef s0
  e <- dupE e0
  (writeOut, out) <- newX s0
  forkIO $ forever $ do
    s <- readIORef ref
    x <- readE e
    let s' = trans x s
    writeIORef ref s'
    writeOut s'
  return out

readX :: X a -> STM a
readX x = case x of
  PureX v -> return v
  FmapX f xx -> f <$> readX xx
  ApplX ff xx -> do
    f <- readX ff
    x <- readX xx
    return (f x)
  PortX tv -> readTVar tv

runDetector :: X a -> (a -> a -> Bool) -> IO (E a)
runDetector x diff = do
  v0 <- atomically (readX x)
  ref <- newIORef v0
  (writeOut, e) <- newE
  forkIO $ forever $ do
    v <- readIORef ref
    v' <- atomically $ do
      v' <- readX x
      when (diff v v' == False) retry
      return v'
    writeIORef ref v'
    writeOut v'
  return e

runEvent :: E a -> (a -> IO ()) -> IO ThreadId
runEvent e0 act = do
  e <- dupE e0
  forkIO $ forever $ do
    x <- readE e
    act x

newX :: a -> IO (a -> IO (), X a)
newX v0 = do
  tv <- atomically (newTVar v0)
  return
    ( \x -> atomically (writeTVar tv x)
    , PortX tv )

newE :: IO (a -> IO (), E a)
newE = do
  ch <- atomically newBroadcastTChan
  return
    ( \x -> atomically (writeTChan ch x)
    , PortE ch )

waitE :: E a -> IO a
waitE e0 = do
  e <- dupE e0
  readE e

hang :: IO a
hang = do
  threadDelay (100 * 10^(6::Int))
  hang

