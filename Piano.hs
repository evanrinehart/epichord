module Piano where

import Data.Monoid

import R2
import Rect
import Chart
import Paint

type Note = Int
data OnOff = On | Off deriving (Eq, Show)
data KeyColor = Wh | Bl deriving (Eq, Show)

data PianoKeys = PianoKeys
  { boundArea :: Rect ()
  , topLinePositions :: (R,R)
  , whiteKeys :: [Rect PianoKey]
  , blackKeys :: [Rect PianoKey] }
    deriving (Eq, Show)

data PianoKey = PianoKey
  { pkOnOff :: OnOff
  , pkColor :: KeyColor
  , pkNote  :: Note }
    deriving (Eq, Show)

pianoChart :: PianoKeys -> Chart R2 PianoKey
pianoChart (PianoKeys area _ whs bls) = 
  let wh = mconcat (map rect whs) in
  let bl = mconcat (map rect bls) in
  clip area (bl <> wh)

pianoView :: PianoKeys -> [Paint]
pianoView (PianoKeys area (tlp1,tlp2) whs bls) =
  [clipOn, blank] ++ colors1 ++ topLines ++ between ++ blacks ++ colors2 where
  clipOn = Clip area
  blank = Fill area (220,220,220)
  colors1 = map (\(Rect _ l t r b) -> Fill (Rect () l t r b) (128,128,128))
    (filter (\(Rect k _ _ _ _) -> pkOnOff k == On) whs)
  between = map (\(Rect _ l _ r b) -> Fill (Rect () l b r (b+1)) (0,0,0)) whs
  topLines =
    [ Fill (Rect () (left area) tlp1 (right area) (tlp1+1)) (0,0,0)
    , Fill (Rect () (left area) tlp2 (right area) (tlp2+1)) (0,0,0) ]
  blacks = map (\r -> Fill (void r) (0,0,0)) bls
  colors2 = map
    (\(Rect _ l t r b) -> Fill (Rect () (l+1) (t+1) (r-1) (b-1)) (200,200,200))
    (filter (\(Rect k _ _ _ _) -> pkOnOff k == On) bls)

count :: [a] -> Int
count = length

whatsOn :: PianoKeys -> [Note]
whatsOn (PianoKeys _ _ whs bls) = map noteOf
  (filter (\(Rect k _ _ _ _) -> pkOnOff k == On) whs ++
  filter (\(Rect k _ _ _ _) -> pkOnOff k == On) bls)

moveKey :: Frame -> R -> Rect a -> Rect a
moveKey (Rect _ l t r b) scroll =
  let heightOfKey = (r - l) / 6 in
  let heightOfKeyboard = heightOfKey * r2f whiteCount in
  let halfViewportHeight = (b - t) / 2 in
  let verticalOffset = scroll*heightOfKeyboard - halfViewportHeight in
  translateRect l t .
  translateRect 0 (-verticalOffset) .
  scaleRect (r - l) heightOfKeyboard

pianoKeys :: Frame -> Double -> [Note] -> PianoKeys
pianoKeys area shift ons = PianoKeys
  { boundArea = area
  , topLinePositions = calculateTlp area shift
  , whiteKeys = map (moveKey area shift) (white ons)
  , blackKeys = map (moveKey area shift) (black ons) }

onTrue :: Bool -> OnOff
onTrue True = On
onTrue False = Off

calculateTlp :: Frame -> R -> (R, R)
calculateTlp (Rect () l t r b) scroll =
  let heightOfKey = (r - l) / 6 in
  let heightOfKeyboard = heightOfKey * r2f whiteCount in
  let halfViewportHeight = (b - t) / 2 in
  let verticalOffset = scroll*heightOfKeyboard - halfViewportHeight in
  let tlp = t - verticalOffset in
  (tlp, tlp - heightOfKey)

white :: [Note] -> [Rect PianoKey]
white ons = zipWith f whiteNotes [0..whiteCount-1] where
  f note i =
    let b = 1 - (r2f i)*whiteHeight in
    let t = 1 - (r2f (i+1))*whiteHeight in
    let pk = PianoKey (onTrue (note `elem` ons)) Wh note in
    Rect pk 0 t 1 b

noteOf :: Rect PianoKey -> Note
noteOf (Rect (PianoKey _ _ note) _ _ _ _) = note

black :: [Note] -> [Rect PianoKey]
black ons =
  let whPlus = map (fmap (\(PianoKey a b c) -> PianoKey a b (c+1))) (white []) in
  let blOnly = filter (isBlack . noteOf) whPlus in
  let fixR (Rect (PianoKey _ _ note) l t _ _) = Rect pk l t' (9/16) b' where
        pk = PianoKey (onTrue (note `elem` ons)) Bl note
        offset = blackOffset (blackClass note)
        b' = t + offset*whiteHeight
        t' = t + offset*whiteHeight - blackWhiteRatio*whiteHeight
  in map fixR blOnly

whiteHeight :: Double
whiteHeight = 1 / (r2f whiteCount)

whiteNotes :: [Int]
whiteNotes = filter isWhite [0..127]

whiteCount :: Int
whiteCount = length whiteNotes

r2f :: Int -> Double
r2f = realToFrac

blackNotes :: [Int]
blackNotes = filter isBlack [0..127]

noteColor :: Note -> KeyColor
noteColor note = case (note-60) `mod` 12 of
  0 -> Wh
  1 -> Bl
  2 -> Wh
  3 -> Bl
  4 -> Wh
  5 -> Wh
  6 -> Bl
  7 -> Wh
  8 -> Bl
  9 -> Wh
  10 -> Bl
  11 -> Wh
  _ -> error "impossible (5)"

isBlack :: Note -> Bool
isBlack note = noteColor note == Bl

isWhite :: Note -> Bool
isWhite note = noteColor note == Wh

blackWhiteRatio :: Double
blackWhiteRatio = 5/8

blackOffset :: Int -> Double
blackOffset n = case n of
  0 -> 7/16
  1 -> 4/16
  2 -> 7/16
  3 -> 5/16
  4 -> 4/16
  _ -> error "impossible (4)"

blackClass :: Note -> Int
blackClass n = case (n - 60) `mod` 12 of
  1 -> 0
  3 -> 1
  6 -> 2
  8 -> 3
  10 -> 4
  _ -> error "impossible (3)"


