module Button where

import StateMachine

data MouseActivity = MClick | MRelease | In | Out deriving Show
data ButtonState = OutUp | InDown | OutDown | InUp deriving Show
data UpDown = Up | Down deriving Show

none :: IO ()
none = return ()

buttonSM :: (UpDown -> IO ()) -> IO () -> StateMachine MouseActivity ButtonState
buttonSM repaint go i st =
  let nil = (st, none) in 
  case st of
    OutUp -> case i of
      In      -> (InUp, none)
      _       -> nil
    InUp -> case i of
      MClick   -> (InDown, repaint Down)
      Out     -> (OutUp, none)
      _       -> nil
    InDown -> case i of
      MRelease -> (InUp, repaint Up >> go)
      Out     -> (OutDown, repaint Up)
      _       -> nil
    OutDown -> case i of
      MRelease -> (OutUp, none)
      In      -> (InDown, repaint Down)
      _       -> nil

