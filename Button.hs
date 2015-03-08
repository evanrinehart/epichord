module Button where

import Canvas
import Mouse

data MA = Click | Release | Enter | Leave deriving Show
data BS = Init | InDown | OutDown deriving Show

fromMouse :: Mouse -> Maybe MA
fromMouse m = case m of
  Motion _ _ _ MouseEnter -> Just Enter
  Motion _ _ _ MouseLeave -> Just Leave
  LeftDown -> Just Click
  LeftUp -> Just Release
  _ -> Nothing

button :: Canvas
       -> (Zone -> BS -> [Paint])
       -> IORef BS
       -> Command
       -> Widget
button canvas look state action = Widget setArea mouse where
  repaint = do
    s <- readIORef state
    d <- getCanvasArea canvas
    canvasPaint canvas (look d s)
  setArea a = do
    s <- readIORef state
    canvasArea canvas a
    canvasPaint canvas (look s)
  mouse m = do
    case fromMouse m of
      Nothing -> return ()
      Just ma -> do
        s <- readIORef state
        let (s', io) = transition repaint action s ma
        writeIORef state s'
        io

noop = return ()

transition :: IO () -> IO () -> BS -> MA -> (BS, IO ())
transition repaint execute s ma = case s of
  Init -> case ma of
    Click   -> (InDown, repaint)
    _       -> (s, noop)
  InDown -> case ma of
    Release -> (Init, repaint >> execute)
    Leave   -> (OutDown, repaint)
    _       -> (s, noop)
  OutDown -> case ma of
    Enter   -> (InDown, repaint)
    Release -> (Init, noop)
    _       -> (s, noop)
