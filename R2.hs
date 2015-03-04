module R2 where

type R2 = (Double, Double)
type N2 = (Int, Int)

(|+|) :: R2 -> R2 -> R2
(a,b) |+| (c,d) = (a+c, b+d)

(|-|) :: R2 -> R2 -> R2
(a,b) |-| (c,d) = (a-c, b-d)

(|*) :: R2 -> Double -> R2
(a,b) |* s = (a*s, b*s)

(*|) :: Double -> R2 -> R2
xy *| s = s |* xy

(|/) :: R2 -> Double -> R2
(a,b) |/ s = (a/s, b/s)

norm :: R2 -> Double
norm (a,b) = sqrt (a*a + b*b)

(.+.) :: N2 -> N2 -> N2
(a,b) .+. (c,d) = (a+c, b+d)

(.-.) :: N2 -> N2 -> N2
(a,b) .-. (c,d) = (a-c, b-d)

(.*) :: N2 -> Int -> N2
(a,b) .* s = (a*s, b*s)

(*.) :: Int -> N2 -> N2
xy *. s = s .* xy

normN2 :: N2 -> Double
normN2 = norm . fromN2

fromR2 :: R2 -> N2
fromR2 (a,b) = (floor a, floor b)

fromN2 :: N2 -> R2
fromN2 (a,b) = (realToFrac a, realToFrac b)
