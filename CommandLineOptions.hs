module CommandLineOptions where

import Options.Applicative
import Data.Monoid

data CommandLineOptions = CommandLineOptions
  { paintToFd :: Maybe Int
  , eventsFromFd :: Maybe Int }
    deriving (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = execParser (info (helper <*> optParser) description)

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
