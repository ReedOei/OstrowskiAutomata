module Sturmian where

import Data.List

p :: [Integer] -> [Integer]
p = tail . tail . p' 0 1
    where
        p' a b (d:ds) = a : p' b (d*b + a) ds

q :: [Integer] -> [Integer]
q = tail . tail . q' 1 0
    where
        q' a b (d:ds) = a : q' b (d*b + a) ds

dif :: [Integer] -> [Integer]
dif ds = zipWith (-) (q ds) $ p ds

rep :: [Integer] -> Integer -> [Integer]
rep reps n = rep' n $ reverse $ takeWhile (<= n) vals
    where
        vals = q reps

        rep' _ [] = []
        rep' x (d:ds)
            | x >= d = let m = x `div` d
                           newX = x - d*m
                       in m : rep' newX ds
            | otherwise = 0 : rep' x ds

zeroEndingParity :: (Eq a, Num a) => [a] -> Bool
zeroEndingParity = (== 0) . (`mod` 2) . genericLength . takeWhile (== 0) . reverse

sturmian :: [Integer] -> [Integer]
sturmian reps = map (go . rep reps) [1..]
    where
        go val = if zeroEndingParity val then 0 else 1

repSturmian :: (Eq a, Num a) => [b] -> [b] -> [a] -> [b]
repSturmian (rep:zRep) oRep (0:xs) = rep : repSturmian zRep oRep xs
repSturmian zRep (rep:oRep) (1:xs) = rep : repSturmian zRep oRep xs

repSturmianWord :: (Integral a, Integral b) => [a] -> [a] -> [a] -> [b]
repSturmianWord zRep oRep = map fromIntegral . repSturmian zRep oRep . sturmian . map fromIntegral

lsdReps :: (Integral a, Integral b) => [a] -> [[[b]]]
lsdReps reps = map (map (:[]) . map fromIntegral . reverse . rep iReps) [1..]
    where
        iReps = map fromIntegral reps

