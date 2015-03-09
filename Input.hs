module Input where

import Debug.Trace (trace)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative hiding ((<|>))
import System.IO
import Control.Monad
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char
import Control.Concurrent

import R2
import qualified Keys as K
import Keys (Key)
import X

data RawInput =
  Mouse Double Double |
  Click MouseButton |
  Release MouseButton |
  KeyDown Key |
  KeyUp Key |
  Character Char |
  Resize Int Int |
  Wheel Double |
  FilePick FilePath |
  MenuNew |
  MenuOpen |
  MenuSave |
  MenuSaveAs |
  MenuAbout |
  Quit
    deriving (Show, Eq, Ord)

newtype MouseButton = MouseButton Int
  deriving (Show, Eq, Ord)

data Raws = Raws
  { rawMouse :: X R2
  , rawWindowSize :: X R2
  , rawClick :: E MouseButton
  , rawRelease :: E MouseButton
  , rawWheel :: E Double
  , rawKeydown :: E Key
  , rawKeyup :: E Key
  , rawChar :: E Char
  , rawQuit :: E () }

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


mouse _ _ x _ y = Mouse x y
click _ _ b = Click b
release _ _ b = Release b
keydown _ _ k = KeyDown k
keyup _ _ k = KeyUp k
character _ _ c = Character c
resize _ _ w _ h = Resize (fromIntegral w) (fromIntegral h)
wheel _ _ z = Wheel z
filePick _ _ s = FilePick s

button :: Parser MouseButton
button = (MouseButton . fromIntegral) <$> decimal

key :: Parser Key
key = do
  s <- getInput
  string s
  case K.fromString s of
    Nothing -> fail "unknown key"
    Just k -> return k

numericChar :: Parser Char
numericChar = do
  n <- decimal
  if n <= 1114111
    then return (chr (fromIntegral n))
    else fail ("invalid character number " ++ show n)

charParser :: Parser Char
charParser = choice
  [ try $ string "\\n" >> pure '\n'
  , try $ string "\\r" >> pure '\r'
  , try $ string "\\t" >> pure '\t'
  , try $ string "\\f" >> pure '\f'
  , try $ string "\\v" >> pure '\v'
  , try $ string "\\b" >> pure '\b'
  , try $ string "\\a" >> pure '\a'
  , try $ char '\\' >> numericChar
  , anyChar ]

parseInputLine :: String -> Maybe RawInput
parseInputLine line = quickParse line $ do
  r <- choice
    [ mouse <$> string "mouse" <*> space <*> float <*> space <*> float
    , try $ click <$> string "click" <*> space <*> button
    , try $ character <$> string "character" <*> space <*> charParser
    , try $ release <$> string "release" <*> space <*> button
    , try $ keydown <$> string "keydown" <*> space <*> key
    , try $ keyup <$> string "keyup" <*> space <*> key
    , resize <$> string "resize" <*> space <*> decimal <*> space <*> decimal
    , wheel <$> string "wheel" <*> space <*> float
    , filePick <$> string "filepick" <*> space <*> getInput
    , MenuNew <$ string "new"
    , MenuOpen <$ string "open"
    , try $ MenuSaveAs <$ string "save-as"
    , MenuSave <$ string "save"
    , MenuAbout <$ string "about"
    , Quit <$ string "quit" ]
  eof
  return r

handleEvents :: Handle -> (RawInput -> IO ()) -> IO ()
handleEvents h eat = forever $ do
  line <- hGetLine h
  case parseInputLine line of
    Nothing -> hPutStrLn stderr ("** CORE unrecognized input " ++ line)
    Just r -> eat r

-- -- --

newRaws :: Handle -> IO (Raws, R2 -> IO ())
newRaws h = do
  (setMouseLoc, mouse) <- newX (0,0)
  (setWindowSize, window) <- newX (0,0)
  (doClick, click) <- newE
  (doRelease, release) <- newE
  (doChar, character) <- newE
  (pressKey, keydown) <- newE
  (releaseKey, keyup) <- newE
  (doWheel, wheel) <- newE
  (doQuit, quit) <- newE
  forkIO $ handleEvents h $ \i -> do
    case i of
      Mouse x y -> setMouseLoc (x,y)
      Resize w h -> setWindowSize (realToFrac w, realToFrac h)
      Click mb -> doClick mb
      Release mb -> doRelease mb
      KeyDown k -> pressKey k
      KeyUp k -> releaseKey k
      Quit -> doQuit ()
      Character c -> doChar c
      Wheel w -> doWheel w
      e -> print e
  return
    ( Raws mouse window click release wheel keydown keyup character quit
    , setWindowSize )
