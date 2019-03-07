module Util where

concats :: [[a]] -> [[a]]
concats [] = [[]]
concats (xs:xss) = [ x:rest | x <- xs, rest <- concats xss ]

