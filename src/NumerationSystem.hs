{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module NumerationSystem where

import Control.Lens

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
        withTransitions = makeTransitions alphabet recogDest states
        states = generalRecogStates maxChar
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, digitAlphabet]

generalRecogStates :: Int -> [State RecogInfo]
generalRecogStates maxChar = initial : zipWith go [1..] stateVals
    where
        initial = State 0 1 [] $ RecogInfo 1 0 0 0
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, digitAlphabet]
        stateVals = concats $ [[0, 1], fracAlphabet] ++ replicate 2 digitAlphabet
        go num l@[isFirst, frac, prev, cur] = State num (isRecog l) [] $ RecogInfo isFirst frac prev cur

isRecog :: [Int] -> Int
isRecog [isFirst, frac, prev, cur]
    | isFirst == 1 = if cur < frac then 1 else 0
    | otherwise = if cur < frac || (cur == frac && prev == 0) then 1 else 0

recogDest :: State RecogInfo -> [Int] -> Maybe (State RecogInfo)
recogDest state [nextFrac, nextDigit]
    | state^.output > 0 = Just $ State (-1) (state^.output) [] $ RecogInfo 0 nextFrac (state^.info.cur) nextDigit
    | otherwise = Nothing

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

