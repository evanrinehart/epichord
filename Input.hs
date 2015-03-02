module Input where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>))
import System.IO
import Control.Monad
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as M

import qualified Keys as K
import Keys (Key)

data RawInput =
  Mouse Double Double |
  Click MouseButton |
  Release MouseButton |
  KeyDown Key |
  KeyUp Key |
  Character Char |
  Resize Int Int |
  Wheel Int |
  Confirm |
  FilePick FilePath |
  MenuNew |
  MenuOpen |
  MenuSave |
  MenuSaveAs |
  MenuAbout |
  MenuQuit
    deriving (Show, Eq, Ord)

data MouseButton = MouseLeft | MouseRight | MouseMiddle
  deriving (Show, Eq, Ord)

quickParse :: String -> Parser a -> Maybe a
quickParse s parser =
  let e = parse parser "raw input" s in
  case e of
    Left _  -> Nothing
    Right r -> Just r

decimal :: Parser Integer
decimal = read <$> many1 digit

decimalFraction :: Parser Rational
decimalFraction = do
  digits <- many1 digit
  let numer = read digits
  let denom = 10^(length digits)
  return (numer % denom)

parseSign :: Num a => Parser (a -> a)
parseSign = (char '-' >> pure negate) <|> pure id

float :: Parser Double
float = do
  sign <- parseSign
  whole <- fromIntegral <$> decimal
  char '.'
  frac <- realToFrac <$> decimalFraction
  return $ sign (whole + frac)


mouse _ _ dx _ dy = Mouse dx dy
click _ _ b = Click b
release _ _ b = Release b
keydown _ _ k = KeyDown k
keyup _ _ k = KeyUp k
character _ _ c = Character c
resize _ _ w _ h = Resize (fromIntegral w) (fromIntegral h)
wheel _ _ z = Wheel (fromIntegral z)
filePick _ _ s = FilePick s

button :: Parser MouseButton
button = choice
  [ MouseLeft <$ string "left"
  , MouseRight <$ string "right"
  , MouseMiddle <$ string "middle" ]

key :: Parser Key
key = do
  s <- getInput
  case K.fromString s of
    Nothing -> fail "unknown key"
    Just k -> return k

parseInputLine :: String -> Maybe RawInput
parseInputLine line = quickParse line $ choice
  [ mouse <$> string "mouse" <*> space <*> float <*> space <*> float
  , try $ click <$> string "click" <*> space <*> button
  , try $ Confirm <$ string "confirm"
  , character <$> string "character" <*> space <*> anyChar
  , release <$> string "release" <*> space <*> button
  , try $ keydown <$> string "keydown" <*> space <*> key
  , keyup <$> string "keyup" <*> space <*> key
  , resize <$> string "resize" <*> space <*> decimal <*> space <*> decimal
  , wheel <$> string "wheel" <*> space <*> decimal
  , filePick <$> string "filepick" <*> space <*> getInput
  , MenuNew <$ string "new"
  , MenuOpen <$ string "open"
  , try $ MenuSaveAs <$ string "save-as"
  , MenuSave <$ string "save"
  , MenuAbout <$ string "about"
  , MenuQuit <$ string "quit" ]

stdinReader :: (RawInput -> IO ()) -> IO a
stdinReader eat = forever $ do
  e <- parseInputLine <$> getLine
  case e of
    Nothing -> hPutStrLn stderr "CORE unrecognized input"
    Just r -> hPutStrLn stderr (show r)


