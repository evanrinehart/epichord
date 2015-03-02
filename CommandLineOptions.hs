module CommandLineOptions where

import System.IO
import System.Posix
import Options.Applicative
import Data.Monoid
import Data.Maybe

data CommandLineOptions = CommandLineOptions
  { paintToFd :: Maybe Int
  , eventsFromFd :: Maybe Int }
    deriving (Show)

parseCommandLineOptions :: IO (Handle, Handle)
parseCommandLineOptions = do
  CommandLineOptions p e <- parseRawCommandLineOptions
  p' <- maybe (return stdout) (fdToHandle . Fd . fromIntegral) p
  e' <- maybe (return stdin)  (fdToHandle . Fd . fromIntegral) e
  hSetEncoding e' utf8
  return (p', e')

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
    optional (option auto eventsFrom)

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
