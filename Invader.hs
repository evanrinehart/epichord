{-# LANGUAGE TupleSections #-}
module Invader where

import Paint
import R2
import Rect

invader :: [[Int]]
invader =
  [[0,0,1,0,0,0,0,0,1,0,0]
  ,[0,0,0,1,0,0,0,1,0,0,0]
  ,[0,0,1,1,1,1,1,1,1,0,0]
  ,[0,1,1,0,1,1,1,0,1,1,0]
  ,[1,1,1,1,1,1,1,1,1,1,1]
  ,[1,0,1,0,0,0,0,0,1,0,1]
  ,[0,0,0,1,1,0,1,1,0,0,0]]

invaderSize :: (Int,Int)
invaderSize = (length (head invader), length invader)

invaderPixels :: [(Int,Int)]
invaderPixels =
  map fst .
  filter ((==1) . snd) .
  concat .
  zipWith (\j x -> zipWith (\i bit -> ((i,j),bit)) [0..] x) [0..] $
  invader

invaderView :: R2 -> Int -> Color -> [Paint]
invaderView (x,y) mult color = pixels where
  pixels = map f invaderPixels
  f ij = Fill (g ij) color
  g (i,j) = Rect () a b c d where
    a = x + rf (i*mult)
    b = y + rf (j*mult)
    c = a + rf mult
    d = b + rf mult

rf :: Int -> Double
rf = realToFrac
