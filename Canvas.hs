module Canvas where

import Control.Monad

import R2
import Paint

{-
canvas is a rectangular region of the window that can show graphics and
be adjusted.
-}

data Canvas = Canvas
  { canvasArea     :: Port Rect
  , canvasPaint    :: Port [Paint]
  , canvasHidden   :: Port Bool
  , getCanvasArea  :: IO Rect }

canvas :: Painter -> IORef Rect -> IORef Bool -> Canvas
canvas paint ref1 ref2 ref3 = Canvas a b c d where
  a r = writeIORef ref1 r
  b cmds = do
    hidden <- readIORef ref3
    unless hidden $
      delta <- readIORef ref1
      -- clip
      paint (map (translatePaint delta) cmds)
      -- unclip
  c hid = writeIORef ref3 hid
  d = readIORef ref1
