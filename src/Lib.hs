{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Lens

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

import System.Environment

import Debug.Trace

import Automata
import Sturmian

data StateInfo = StateInfo
    { _zDiff :: Int
    , _oDiff :: Int
    , _seqNum :: Int
    , _seqLen :: Int
    , _isEven :: Bool
    , _pastStart :: Bool
    , _zMod :: Int
    , _oMod :: Int }
    deriving (Show, Eq)
makeLenses ''StateInfo

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs =
    let (c, rest) = splitAt n xs
    in c : chunk n rest

periodic :: Eq a => [a] -> [a]
periodic xs = fromMaybe xs $ find (\periodicPart -> and $ zipWith (==) (cycle periodicPart) xs) $ tail $ inits xs

computePeriod :: Eq a => [a] -> ([a], [a])
computePeriod xs = snd $ minimumBy (comparing fst) $ go [] xs
    where
        go fullList [] = [(length fullList, (fullList, []))]
        go nonRepeat repeating@(y:ys) = (totalLen, period) : go (nonRepeat ++ [y]) ys
            where
                period = (nonRepeat, periodicPart)
                totalLen = length nonRepeat + length periodicPart
                periodicPart = periodic repeating

computeZPeriod :: [Integer] -> Integer -> ([Integer], [Integer])
computeZPeriod ds m = computePeriod $ take 3000 $ map (`mod` m) $ dif ds

computeOPeriod :: [Integer] -> Integer -> ([Integer], [Integer])
computeOPeriod ds m = computePeriod $ take 3000 $ map (`mod` m) $ p ds

transitionDest :: State StateInfo -> [Int] -> Int -> Int -> State StateInfo
transitionDest st [letter] n m =
    over info (set isEven newEven .
    set zMod newZMod .
    set pastStart newPastStart .
    set seqNum newSeqNum .
    set oMod newOMod) st
    where
        i = st^.info
        newEven
            | i^.pastStart = i^.isEven
            | letter == 0 = not $ i^.isEven
            | otherwise = i^.isEven
        newPastStart = i^.pastStart || letter /= 0
        newSeqNum = (i^.seqNum + 1) `mod` (i^.seqLen)
        newZMod = (letter * i^.zDiff + i^.zMod) `mod` n
        newOMod = (letter * i^.oDiff + i^.oMod) `mod` m

genAutomata :: [[Int]] -> ([Int], [Int]) -> ([Int], [Int]) -> [Int] -> [Int] -> [State StateInfo]
genAutomata alphabet zPeriod oPeriod zRep oRep =
    minimizeAutomata alphabet $
    prune $
    genNonRepTrans n m alphabet nonRepStates repStates ++
    genTransitions n m alphabet repStates repStates
    where
        n = genericLength zRep
        m = genericLength oRep
        (nonRepeat, repeat) = makePeriods zPeriod oPeriod
        (nonRepStates, repStates) = genStates nonRepeat repeat zRep oRep

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith (flip const) xs $ drop n $ cycle xs

makePeriods (zNonRep, zRep) (oNonRep, oRep) = (nonRepeat, rep)
    where
        nonRepeat
            | length zNonRep > length oNonRep = zip zNonRep $ oNonRep ++ concat (repeat oRep)
            | otherwise = zip (zNonRep ++ concat (repeat zRep)) oNonRep
        newZRep = rotate (length nonRepeat - length zNonRep) zRep
        newORep = rotate (length nonRepeat - length oNonRep) oRep
        rep = zipUntilRepeat (max (length newZRep) (length newORep)) (cycle newZRep) $ cycle newORep

zipUntilRepeat minL = zipUntilRepeat' []
    where
        zipUntilRepeat' acc _ [] = acc
        zipUntilRepeat' acc [] _ = acc
        zipUntilRepeat' acc (x:xs) (y:ys)
            | (x, y) `elem` acc && length acc >= minL = acc
            | otherwise = zipUntilRepeat' (acc ++ [(x,y)]) xs ys

genNonRepTrans :: Int -> Int -> [[Int]] -> [State StateInfo] -> [State StateInfo] -> [State StateInfo]
genNonRepTrans n m alphabet nonRepStates repStates =
    zipWith (flip (transitionsForState n m alphabet)) nonRepStates $ tail $ tails $ nonRepStates ++ repStates

genTransitions n m alphabet destStates = map (transitionsForState n m alphabet destStates)

transitionsForState n m alphabet destStates state =
    set transitions stTransitions state
    where
        stTransitions = map go alphabet
        go letter = Transition letter $ newSt^.num
            where newSt = findDest destStates $ transitionDest state letter n m

findDest :: Foldable t => t (State StateInfo) -> State StateInfo -> State StateInfo
findDest states state =
    case find (stateMatching state) states of
        Just st -> st

stateMatching st check =
    let i = st^.info
        ci = check^.info
    in i^.isEven == ci^.isEven &&
       i^.zMod == ci^.zMod &&
       i^.pastStart == ci^.pastStart &&
       i^.seqNum == ci^.seqNum &&
       i^.oMod == ci^.oMod

genStates nonRepeat repeat zRep oRep = (nonRepStates, repStates)
    where
        n = genericLength zRep
        m = genericLength oRep
        (nonRepStates, k) = genStatesWithDiffs 0 nonRepeat zRep oRep
        (repStates, _) = genStatesWithDiffs k repeat zRep oRep

genStatesWithDiffs startNum diffs zRep oRep = (zipWith go [startNum..] stateInfo, endNum)
    where
        endNum = startNum + genericLength stateInfo
        stateInfo = [ (seqNum, diff, isEven, pastStart, zMod, oMod) | (seqNum, diff) <- zip [0..] diffs,
                                                                      isEven <- [True, False],
                                                                      pastStart <- [False, True],
                                                                      zMod <- [0..n - 1],
                                                                      oMod <- [0..m - 1]]
        n = genericLength zRep
        m = genericLength oRep
        go num (seqNum, (zDiff, oDiff), isEven, pastStart, zMod, oMod) = State num out [] $ StateInfo zDiff oDiff seqNum (length diffs) isEven pastStart zMod oMod
            -- Need to shift by one here because the formula gives us the number including the current position
            where out = if isEven then zRep !! ((zMod - 1) `mod` n) else oRep !! ((oMod - 1) `mod` m)

makeAutomata :: [[Int]] -> [Int] -> [Int] -> [Integer] -> [State StateInfo]
makeAutomata alphabet zRep oRep reps = genAutomata alphabet zPeriod oPeriod zRep oRep
    where
        zPeriod = (map fromIntegral zNonRep, map fromIntegral zRepPart)
        oPeriod = (map fromIntegral oNonRep, map fromIntegral oRepPart)
        (zNonRep, zRepPart) = computeZPeriod reps $ genericLength zRep
        (oNonRep, oRepPart) = computeOPeriod reps $ genericLength oRep

makeOutputAutomata alphabet zRep oRep reps =
    map (\outputVal -> (outputVal, outputAutomataToAcceptAutomata outputVal states)) $ nub $ zRep ++ oRep
    where
        states = makeAutomata alphabet zRep oRep reps

