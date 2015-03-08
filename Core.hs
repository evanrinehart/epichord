module Main where

import System.IO
import Control.Exception
import Control.Monad
import System.Exit
import Control.Concurrent

import Control.Applicative

import Data.Monoid
import Data.Ix

import Input
import Config
import Paint
import X
--import Tracker
--import Button
import StateMachine
import Chart
import Rect
import R2

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  let painter = newPaintOut paintOutH
  let flush = hPutStrLn paintOutH "flush"
  --forkIO (keepAlive painter)
  putStrLn "CORE Hello World"
  raws <- newRaws (640, 480) eventInH
  let mouse = rawMouse raws
  let window = rawWindowSize raws
  let quit = rawQuit raws
  look <- runDetector (simonView <$> pure (0,0) <*> window <*> pure Nothing) (/=)
  onE look (\cmds -> painter cmds >> flush)
  --onE sideChanged print
  waitE quit

simonView :: Z2 -> Z2 -> Maybe Int -> [Paint]
simonView (x,y) (w,h) color = [c1, c2, c3, c4] where
  w2 = w `div` 2
  h2 = h `div` 2
  mx = x + w `div` 2
  my = y + h `div` 2
  calc 0 (Just 0) = (255, 0, 0)
  calc 1 (Just 1) = (255, 255, 0)
  calc 2 (Just 2) = (0, 0, 255)
  calc 3 (Just 3) = (0, 255, 0)
  calc 0 _ = (128, 0, 0)
  calc 1 _ = (128, 128, 0)
  calc 2 _ = (0, 0, 128)
  calc 3 _ = (0, 128, 0)
  c1 = Fill (Rect x y w2 h2) (calc 0 color)
  c2 = Fill (Rect mx y w2 h2) (calc 1 color)
  c3 = Fill (Rect x my w2 h2) (calc 2 color)
  c4 = Fill (Rect mx my w2 h2) (calc 3 color)

lr :: X (Double, Double) -> X (Int, Int) -> X Bool
lr mouse window = liftA2 f mouse window where
  f (x,y) (w,h) = let x' = floor x in
                  (x' < w `div` 2)

--newRepainter :: Eq a => Painter -> X a -> (a -> [Paint]) -> IO ()

keepAlive :: ([Paint] -> IO ()) -> IO ()
keepAlive paint = forever $ do
  threadDelay 10000000
  paint []

