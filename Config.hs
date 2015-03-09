module Config where

import System.IO
import System.Posix
import Options.Applicative
import Data.Monoid
import Data.Maybe

data CommandLineOptions = CommandLineOptions
  { paintToFd :: Maybe Int
  , eventsFromFd :: Maybe Int
  , windowDimensions :: (Int,Int) }
    deriving (Show)

parseCommandLineOptions :: IO (Handle, Handle, (Int,Int))
parseCommandLineOptions = do
  CommandLineOptions p e dim <- parseRawCommandLineOptions
  p' <- maybe (return stdout) (fdToHandle . Fd . fromIntegral) p
  e' <- maybe (return stdin)  (fdToHandle . Fd . fromIntegral) e
  hSetEncoding e' utf8
  hSetEncoding p' utf8
  hSetBuffering p' LineBuffering
  return (p', e', dim)

parseRawCommandLineOptions :: IO CommandLineOptions
parseRawCommandLineOptions = execParser (info (helper <*> optParser) description)

description =
  progDesc "Epichord's core application program." <>
  fullDesc <>
  (header $ unwords
    ["By default communication with a GUI is via stdio."
    ,"There are options to make it use file descriptor numbers of choice."]) <>
  failureCode (-1)

optParser =
  CommandLineOptions <$>
    optional (option auto paintTo) <*>
    optional (option auto eventsFrom) <*>
    (option auto windowDimensionsParser)

paintTo = 
  long "paint-to" <>
  short 'p' <>
  metavar "FD" <>
  help "File descriptor provided by parent process for paint commands"

eventsFrom =
  long "events-from" <>
  short 'e' <>
  metavar "FD" <>
  help "File descriptor provided by parent process for reading input events"

windowDimensionsParser =
  long "window" <>
  short 'w' <>
  metavar "(W,H)" <>
  help "Initial dimensions of the window"
