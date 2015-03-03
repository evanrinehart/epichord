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
newPaintOut h = return $ \msg -> do
  hPutStrLn h msg

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
  handleEvents eventInH $ \i -> do
    case i of
      Mouse x y -> do
        paintOut $ unwords
          ["fill", show $ floor(x-50), show $ floor(y-50), "100", "100",
           "0", "255", "0"]
        paintOut "flush"
      _ -> return ()
