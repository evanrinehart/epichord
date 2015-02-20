module Main where

import Control.Concurrent
import System.IO

main = do
  hPutStrLn stderr "COR hello world"
--  x <- getLine
--  hPputStrLn stderr ("COR i heard " ++ x)
  threadDelay (5 * 10^6)
