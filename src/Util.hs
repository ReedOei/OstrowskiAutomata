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

untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange f x
    | newVal == x = x
    | otherwise = untilNoChange f newVal
    where newVal = f x

makeAlphabetStr :: [[Int]] -> String
makeAlphabetStr = unwords . map go
    where
        go alphabet = "{" ++ intercalate "," (map show alphabet) ++ "}"

padRight :: Int -> a -> [a] -> [a]
padRight minLen e xs = xs ++ replicate (minLen - length xs) e

baseNInput :: Int -> Int -> [[Int]] -> [[Int]]
baseNInput base maxLen input = concatMap (padRight maxLen padChar . go) input
    where
        padChar :: [Int]
        padChar = replicate (length (head input)) 0

        go vals
            | all (< base) vals = [vals]
            | otherwise = ms : go ds
                where
                    (ds, ms) = unzip $ map (`divMod` base) vals

