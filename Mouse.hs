module Mouse where

import R2

data UpDown = Up | Down deriving Show

data Mouse =
  Motion R2 R2 [UpDown] MouseTransfer |
  LeftDown R2 |
  LeftUp R2 |
  RightClick R2 |
  MiddleClick R2 |
  Wheel Double R2 |
  DoubleClick R2
    deriving Show

data MouseTransfer = MouseEnter | MouseLeave | NoTransfer deriving Show

mouseLocation :: Mouse -> R2
mouseLocation m = case m of
  Motion _ xy _ _ -> xy
  LeftDown xy    -> xy
  LeftUp xy      -> xy
  RightClick xy  -> xy
  MiddleClick xy -> xy
  Wheel _ xy     -> xy
  DoubleClick xy -> xy

translateMouse :: Mouse -> R2 -> Mouse
translateMouse m delta = case m of
  Motion xy0 xy1 bs tr -> Motion (xy0 |+| delta) (xy1 |+| delta) bs tr
  LeftDown xy    -> LeftDown (xy |+| delta)
  LeftUp xy      -> undefined
  RightClick xy  -> undefined
  MiddleClick xy -> undefined
  Wheel _ xy     -> undefined
  DoubleClick xy -> DoubleClick (xy |+| delta)

