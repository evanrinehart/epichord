module Keys where

import Data.Char

data Key =
 Escape | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 |
 Power | GraveAccent | One | Two | Three | Four | Five | Six | Seven | Eight |
 Nine | Zero | Minus | Equals | Backspace | Tab | Q | W | E | R | T | Y |
 U | I | O | P | LeftBracket | RightBracket | Backslash | CapsLock |
 A | S | D | F | G | H | J | K | L | Semicolon | Quote | Return |
 LeftShift | Z | X | C | V | B | N | M | Comma | Period | Slash | RightShift |
 Fn | LeftControl | LeftOption | LeftCommand | Spacebar | Unknown String Int |
 RightCommand | RightOption | LeftArrow | RightArrow | UpArrow | DownArrow
  deriving (Show, Eq, Ord)

fromString :: String -> Maybe Key
fromString s = case s of
  "escape" -> Just Escape
  "f1" -> Just F1
  "f2" -> Just F2
  "f3" -> Just F3 
  "f4" -> Just F4 
  "f5" -> Just F5 
  "f6" -> Just F6 
  "f7" -> Just F7 
  "f8" -> Just F8 
  "f9" -> Just F9 
  "f10" -> Just F10 
  "f11" -> Just F11 
  "f12" -> Just F12 
  "power" -> Just Power 
  "grave-accent" -> Just GraveAccent
  "1" -> Just One 
  "2" -> Just Two 
  "3" -> Just Three 
  "4" -> Just Four 
  "5" -> Just Five 
  "6" -> Just Six 
  "7" -> Just Seven 
  "8" -> Just Eight 
  "9" -> Just Nine 
  "0" -> Just Zero 
  "minus" -> Just Minus
  "equals" -> Just Equals 
  "backspace" -> Just Backspace
  "tab" -> Just Tab 
  "q" -> Just Q 
  "w" -> Just W 
  "e" -> Just E 
  "r" -> Just R 
  "t" -> Just T 
  "y" -> Just Y 
  "u" -> Just U 
  "i" -> Just I 
  "o" -> Just O 
  "p" -> Just P 
  "left-bracket" -> Just LeftBracket 
  "right-bracket" -> Just RightBracket 
  "backslash" -> Just Backslash 
  "caps-lock" -> Just CapsLock 
  "a" -> Just A 
  "s" -> Just S 
  "d" -> Just D 
  "f" -> Just F 
  "g" -> Just G 
  "h" -> Just H 
  "j" -> Just J 
  "k" -> Just K 
  "l" -> Just L 
  "semicolon" -> Just Semicolon 
  "quote" -> Just Quote 
  "return" -> Just Return 
  "left-shift" -> Just LeftShift 
  "z" -> Just Z 
  "x" -> Just X 
  "c" -> Just C 
  "v" -> Just V 
  "b" -> Just B 
  "n" -> Just N 
  "m" -> Just M 
  "comma" -> Just Comma 
  "period" -> Just Period 
  "slash" -> Just Slash 
  "right-shift" -> Just RightShift 
  "fn" -> Just Fn 
  "left-control" -> Just LeftControl 
  "left-option" -> Just LeftOption 
  "left-command" -> Just LeftCommand 
  "spacebar" -> Just Spacebar 
  "right-command" -> Just RightCommand 
  "right-option" -> Just RightOption 
  "left-arrow" -> Just LeftArrow 
  "right-arrow" -> Just RightArrow 
  "up-arrow" -> Just UpArrow 
  "down-arrow" -> Just DownArrow
  ('u':'n':'k':'n':'o':'w':'n':' ':ss) -> 
    let (system, sss) = break isSpace ss in
    case reads sss of
      [] -> Nothing
      [(code, _)] -> Just (Unknown system code)
      _ -> Nothing
  _ -> Nothing

