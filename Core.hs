module Main where

import System.IO
import System.Posix
import Control.Exception
import Control.Monad

import Input
import CommandLineOptions

whenJust :: Maybe a -> (a -> IO b) -> IO ()
whenJust Nothing  _ = return ()
whenJust (Just x) f = f x >> return ()

newPaintOut :: Handle -> IO (String -> IO ())
newPaintOut h = return (\msg -> hPutStrLn h msg)

handleEvents :: Handle -> (RawInput -> IO ()) -> IO ()
handleEvents h eat = forever $ do
  line <- hGetLine h
  case parseInputLine line of
    Nothing -> hPutStrLn stderr ("CORE unrecognized input " ++ line)
    Just r -> eat r

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  paintOut <- newPaintOut paintOutH
  handleEvents eventInH (putStrLn . show)
