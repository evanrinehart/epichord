module Main where

import Control.Concurrent
import System.IO

main = do
  hPutStrLn stderr "CORE Hello World"
--  x <- getLine
--  hPputStrLn stderr ("COR i heard " ++ x)
  threadDelay (5 * 10^6)
  hPutStrLn stderr "CORE end of"
