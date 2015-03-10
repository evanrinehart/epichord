{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module X (
  X,
  E,
  Output,
  newX,
  newE,
  snapshot,
  snapshot_,
  filterE,
  justE,
  maybeE,
  edge,
  accumulate,
  out,
  runProgram,
  debugX,
  debugE
) where

import Control.Applicative
import Data.Functor
import Data.Monoid
import Control.Monad
import Data.Unamb
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Data.Function
import Data.Time
import System.IO.Unsafe

data X a where
  PureX :: a -> X a
  FmapX :: forall a b . (b -> a) -> X b -> X a
  ApplX :: forall a b . X (b -> a) -> X b -> X a
  PortX :: TVar a -> X a

data E a where
  NeverE    :: E a
  FmapE     :: forall a b . (b -> a) -> E b -> E a
  MappendE  :: E a -> E a -> E a
  ProductE  :: (b -> c -> a) -> E b -> E c -> E a
  SnapshotE :: E b -> X a -> E a
  PortE     :: TChan a -> E a
  JustE     :: E (Maybe a) -> E a

type Output = IO ()

instance Functor X where
  fmap f x = FmapX f x

instance Applicative X where
  pure x = PureX x
  f <*> x = ApplX f x

instance Functor E where
  fmap f e = FmapE f e

instance Monoid (E a) where
  mempty = NeverE
  mappend e1 e2 = MappendE e1 e2

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
  JustE e' -> do
    e'' <- dupE e'
    return (JustE e'')

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
  JustE e' -> fix $ \loop -> do
    m <- readE e'
    case m of
      Nothing -> loop
      Just x  -> return x

readX :: X a -> STM a
readX x = case x of
  PureX v -> return v
  FmapX f xx -> f <$> readX xx
  ApplX ff xx -> do
    f <- readX ff
    x <- readX xx
    return (f x)
  PortX tv -> readTVar tv

hang :: IO a
hang = do
  threadDelay (100 * 10^(6::Int))
  hang

diff :: Eq a => a -> a -> Maybe a
diff a b = if a == b then Nothing else Just b

forkOut :: Output -> IO ()
forkOut act = do
  forkIO act
  return ()

---

newX :: a -> IO (a -> IO (), X a)
newX v0 = do
  tv <- newTVarIO v0
  return
    ( \x -> atomically (writeTVar tv x)
    , PortX tv )

newE :: IO (a -> IO (), E a)
newE = do
  ch <- newBroadcastTChanIO
  return
    ( \x -> atomically (writeTChan ch x)
    , PortE ch )

snapshot :: E a -> X b -> E (a,b)
snapshot e x = ProductE (,) e (SnapshotE e x)

snapshot_ :: E a -> X b -> E b
snapshot_ e x = SnapshotE e x

justE :: E (Maybe a) -> E a
justE = JustE

maybeE :: (a -> Maybe b) -> E a -> E b
maybeE f e = justE (f <$> e)

filterE :: (a -> Bool) -> E a -> E a
filterE p e = maybeE (\x -> if p x then Just x else Nothing) e

edge :: X a -> (a -> a -> Maybe b) -> E b
edge x diff = PortE ch where
  ch = unsafePerformIO $ do
    out <- newBroadcastTChanIO
    forkIO $ do
      v0 <- atomically (readX x)
      ref <- newIORef v0
      forever $ do
        v <- readIORef ref
        (d, v') <- atomically $ do
          v' <- readX x
          case diff v v' of
            Just d  -> return (d, v')
            Nothing -> retry
        writeIORef ref v'
        atomically (writeTChan out d)
    return out

out :: E a -> (a -> IO ()) -> Output
out e0 act = do
  e <- dupE e0
  forever (readE e >>= act)

accumulate :: E a -> s -> (a -> s -> s) -> X s
accumulate e0 s0 trans = PortX tv where
  tv = unsafePerformIO $ do
    state <- newTVarIO s0
    forkIO $ do
      e <- dupE e0
      forever $ do
        x <- readE e
        atomically $ do
          s <- readTVar state
          let s' = trans x s
          writeTVar state s'
    return state

waitE :: E a -> IO a
waitE e0 = do
  e <- dupE e0
  readE e

debugX :: (Eq a, Show a) => X a -> Output
debugX x = do
  v0 <- atomically (readX x)
  print v0
  out (edge x diff) print

debugE :: (Show a) => E a -> Output
debugE e = do
  out e print

runProgram :: IO () -> E () -> [Output] -> IO ()
runProgram notifyBoot stop outs = do
  forM_ outs forkOut
  threadDelay 5000
  notifyBoot
  waitE stop
