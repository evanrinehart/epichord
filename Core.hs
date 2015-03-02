module Main where

import System.IO
import System.Posix
import Control.Exception

import Input
import CommandLineOptions

whenJust :: Maybe a -> (a -> IO b) -> IO ()
whenJust Nothing  _ = return ()
whenJust (Just x) f = f x >> return ()

main = do
  opts <- parseCommandLineOptions
  hPutStrLn stderr "CORE Hello World"
  print opts
  --Right smf <- fmap canonical <$> readMidi "midis/windfis2.mid"
--  print (mf_header smf)
--  putAscii smf
 -- dumpMidiFile "1234" "1234" smf
  finally (stdinReader (\x -> print x)) $ do
    hPutStrLn stderr "** CORE exception while processing input"
  hPutStrLn stderr "** CORE the impossible has occurred"
