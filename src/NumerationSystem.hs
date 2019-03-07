{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module NumerationSystem where

import Control.Lens

import Data.Function

import Automata
import Util

data RecogInfo = RecogInfo
    { _isFirst :: Int
    , _frac :: Int
    , _prev :: Int
    , _cur :: Int }
    deriving (Show, Eq)
makeLenses ''RecogInfo

generalRecogAutomata :: Int -> [State RecogInfo]
generalRecogAutomata maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        withTransitions = makeTransitions alphabet (recogDest maxChar) states
        states = generalRecogStates maxChar
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = map ((:[]) . encode maxChar) $ concats [fracAlphabet, digitAlphabet]

generalRecogStates :: Int -> [State RecogInfo]
generalRecogStates maxChar = initial : zipWith go [1..] stateVals
    where
        initial = State 0 1 [] $ RecogInfo 1 0 0 0
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        stateVals = concats $ [[0, 1], fracAlphabet] ++ replicate 2 digitAlphabet
        go num l@[isFirst, frac, prev, cur] = State num (isRecog l) [] $ RecogInfo isFirst frac prev cur

encode :: Int -> [Int] -> Int
encode maxChar [frac, digit] = frac*maxChar + digit

decode :: Int -> Int -> [Int]
decode maxChar encoded = [encoded `div` maxChar, encoded `mod` maxChar]

isRecog :: [Int] -> Int
isRecog [isFirst, frac, prev, cur]
    | isFirst == 1 = if cur < frac then 1 else 0
    | otherwise = if cur < frac || (cur == frac && prev == 0) then 1 else 0

recogDest :: Int -> State RecogInfo -> [Int] -> Maybe (State RecogInfo)
recogDest maxChar state [encoded]
    | state^.output > 0 = Just $ State (-1) (state^.output) [] $ RecogInfo 0 nextFrac (state^.info.cur) nextDigit
    | otherwise = Nothing
    where [nextFrac, nextDigit] = decode maxChar encoded

data WordInfo = WordInfo
    { _isEven :: Bool
    , _pastStart :: Bool }
    deriving (Show, Eq)
makeLenses ''WordInfo

generalWord :: Int -> [State WordInfo]
generalWord maxChar =
    minimizeAutomata alphabet $ prune $
    makeTransitionsBy alphabet ((==) `on` (^.info)) (wordDest maxChar) generalWordStates
    where
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = map ((:[]) . encode maxChar) $ concats [fracAlphabet, digitAlphabet]

generalWordStates = zipWith go [0..] [(isEven, pastStart) | isEven <- [True, False], pastStart <- [False, True]]
    where
        go num (isEven, pastStart) = State num (if isEven then 0 else 1) [] $ WordInfo isEven pastStart

wordDest :: Int -> State WordInfo -> [Int] -> Maybe (State WordInfo)
wordDest maxChar state [encoded] = Just $ State (-1) (-1) [] $ WordInfo newEven newPastStart
    where
        [nextFrac, nextDigit] = decode maxChar encoded
        i = state^.info
        newEven
            | i^.pastStart = i^.isEven
            | nextDigit == 0 = not $ i^.isEven
            | otherwise = i^.isEven
        newPastStart = i^.pastStart || nextDigit /= 0

-- data StateInfo = StateInfo
--     { _u :: (Integer, Integer, Integer)
--     , _v :: (Integer, Integer, Integer)
--     , _w :: (Integer, Integer, Integer)
--     , _g :: Integer } -- This represents the shift in the last value in the window, from the rules of the
--     deriving (Show, Eq)
-- makeLenses ''StateInfo

-- algorithm1States :: Integer -> [State StateInfo]
-- algorithm1States maxChar = makeStates [1..maxChar] [0..maxChar]
--     where
--         makeStates fracAlphabet digitAlphabet = zipWith go [1..] $ concats $ replicate 6 digitAlphabet ++ replicate 3 fracAlphabet ++ [[0,1]]
--             where
--                 go num l@[v1, v2, v3, w1, w2, w3, u1, u2, u3, g] =
--                     State num (isFinal l) [] $ StateInfo (u1,u2,u3) (v1,v2,v3) (w1,w2,w3) g

-- isFinal :: [Integer] -> Integer
-- isFinal

