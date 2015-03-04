module Main where

import System.IO
import Control.Exception
import Control.Monad
import System.Exit

import Input
import Config
import Paint

whenJust :: Maybe a -> (a -> IO b) -> IO ()
whenJust Nothing  _ = return ()
whenJust (Just x) f = f x >> return ()

handleEvents :: Handle -> (RawInput -> IO ()) -> IO ()
handleEvents h eat = forever $ do
  line <- hGetLine h
  case parseInputLine line of
    Nothing -> hPutStrLn stderr ("** CORE unrecognized input " ++ line)
    Just r -> eat r

main = do
  (paintOutH, eventInH) <- parseCommandLineOptions
  putStrLn "CORE Hello World"
  paintOut <- newPaintOut paintOutH
  handleEvents eventInH $ \i -> do
    case i of
      Mouse x y -> paintOut
        [Fill (Rect (floor x-50) (floor y-50) 100 100) (0,255,0)]
      Quit -> exitSuccess
      _ -> print i
