module Util where

import Data.List

concats :: [[a]] -> [[a]]
concats [] = [[]]
concats (xs:xss) = [ x:rest | x <- xs, rest <- concats xss ]

borders :: Eq a => [a] -> [[a]]
borders xs = filter (not . null) $ filter (\b -> b `isSuffixOf` xs) $ takeWhile (\b -> 2 * length b <= length xs) $ inits xs

bordered :: Eq a => [a] -> Bool
bordered = not . null . borders

unbordered :: Eq a => [a] -> Bool
unbordered = not . bordered

substrings :: [a] -> [[a]]
substrings = filter (not . null) . concatMap inits . tails

