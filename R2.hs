module R2 where

type R = Double
type R2 = (R,R)
type Z2 = (Int, Int)
type N2 = Z2

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

(.+.) :: Z2 -> Z2 -> Z2
(a,b) .+. (c,d) = (a+c, b+d)

(.-.) :: Z2 -> Z2 -> Z2
(a,b) .-. (c,d) = (a-c, b-d)

(.*) :: Z2 -> Int -> Z2
(a,b) .* s = (a*s, b*s)

(*.) :: Int -> Z2 -> Z2
xy *. s = s .* xy

normZ2 :: Z2 -> Double
normZ2 = norm . fromZ2

fromR2 :: R2 -> Z2
fromR2 (a,b) = (floor a, floor b)

fromZ2 :: Z2 -> R2
fromZ2 (a,b) = (realToFrac a, realToFrac b)
