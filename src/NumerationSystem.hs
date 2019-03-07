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
    deriving (Show, Eq, Ord)
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
        stateVals = concats $ [[0, 1], fracAlphabet] ++ replicate 2 digitAlphabet
        go num l@[isFirst, frac, prev, cur] = State num (isRecog l) [] $ RecogInfo isFirst frac prev cur

isRecog :: [Int] -> Int
isRecog [isFirst, frac, prev, cur]
    | isFirst == 1 = if cur < frac then 1 else 0
    | otherwise = if cur < frac || (cur == frac && prev == 0) then 1 else 0

recogDest :: State RecogInfo -> [Int] -> Maybe (Int, RecogInfo)
recogDest state [nextFrac, nextDigit]
    | state^.output > 0 = Just $ (state^.output, RecogInfo 0 nextFrac (state^.info.cur) nextDigit)
    | otherwise = Nothing

alg0Automaton :: Int -> [State ()]
alg0Automaton maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        digitAlphabet = [0..maxChar]
        sumAlphabet = [0..maxChar + maxChar]
        alphabet = concats [digitAlphabet, digitAlphabet, sumAlphabet]
        withTransitions = makeTransitions alphabet alg0Dest alg0States

-- This automata always has just one state, anything that would transition it to an invalid state is just excluded
alg0States :: [State ()]
alg0States = [State 0 1 [] ()]

alg0Dest :: State () -> [Int] -> Maybe (Int, ())
alg0Dest state [a, b, c]
    | a + b == c = Just $ (1, ())
    | otherwise = Nothing

data Alg1Info = Alg1Info
    { _u :: (Int, Int, Int)
    , _v :: (Int, Int, Int)
    , _w :: (Int, Int, Int)
    , _g :: Int } -- This represents the shift in the last value in the window, from the rules of the
    deriving (Show, Eq, Ord)
makeLenses ''Alg1Info

-- Scans Left to Right -- i.e., MSD
alg1Automaton :: Int -> [State Alg1Info]
alg1Automaton maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        fracAlphabet = [1..maxChar]
        sumAlphabet = [0..maxChar + maxChar] -- These symbols come from directly summing the two strings, so the range is larger
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, sumAlphabet, digitAlphabet]
        withTransitions = makeTransitionsBy alphabet (^.info) alg1Dest states
        states = alg1States maxChar

alg1States :: Int -> [State Alg1Info]
alg1States maxChar = makeStates
    where
        -- Intentionally include 0 here, even though it's not a valid input, for the initial states
        fracAlphabet = [0..maxChar]
        digitAlphabet = [0..maxChar]
        sumAlphabet = [0..maxChar + maxChar] -- These symbols come from directly summing the two strings, so the range is larger
        initialInfo = Alg1Info (0,0,0) (0,0,0) (0,0,0) 0
        initialInfoList = [0,0,0,0,0,0,0,0,0,0]
        initial = State 0 (isFinalAlg1 initialInfoList) [] initialInfo
        makeStates = initial : zipWith go [1..] (concats (replicate 3 sumAlphabet ++ replicate 3 digitAlphabet ++ replicate 3 fracAlphabet ++ [[0,1]]))
            where
                go num l@[v1, v2, v3, w1, w2, w3, u1, u2, u3, g] =
                    State num (isFinalAlg1 l) [] $ Alg1Info (u1,u2,u3) (v1,v2,v3) (w1,w2,w3) g

isFinalAlg1 :: [Int] -> Int
isFinalAlg1 [v1, v2, v3, w1, w2, w3, u1, u2, u3, g] =
    let boolVal
            -- Revert A1
            | g == 1 = isFinalAlg1 [v1 - 1, v2 + u2 + 1, 0, w1, w2, w3, u1, u2, u3, 0] == 1
            -- B1
            | v1 < u1 && v2 > u2 && v3 == 0 = (w1 == v1+1) && (w2 == v2-u2-1) && (w3 == u3-1)
            -- B2
            | v1 < u1 && v2 >= u2 && v3 > 0 && v3 <= u3 = (w1 == v1+1) && (w2 == v2-u2) && (w3 == v3-1)
            -- B3
            | v1 < u1 && v2 >= u2 && v3 > u3 = (w1 == v1+1) && (w2 == v2-u2+1) && (w3 == v3-u3-1)
            -- B4
            | v2 < u2 && v3 >= u3 = (w1 == v1) && (w2 == v2+1) && (w3 == v3-u3)
            -- B5
            | otherwise = (w1 == v1) && (w2 == v2) && (w3 == v3)
    in if boolVal then 1 else 0

alg1Dest :: State Alg1Info -> [Int] -> Maybe Alg1Info
alg1Dest state [u4, v4', w4]
    | v2 < u2 && v3 > u3 && v4 == 0 && v1 == w1 = Just $ Alg1Info (u2,u3,u4) (v2+1,v3-(u3+1),u4-1) (w2,w3,w4) 1
    | v2 < u2 && v3 >= u3 && v3 <= 2*u3 && v3 > 0 && v1 == w1 = Just $ Alg1Info (u2,u3,u4) (v2+1,v3-u3,v4-1) (w2,w3,w4) 0
    | v1 == w1 = Just $ Alg1Info (u2,u3,u4) (v2,v3,v4) (w2,w3,w4) 0
    | otherwise = Nothing
    where
        (u1,u2,u3) = state^.info.u
        (v1,v2,v3) = state^.info.v
        (w1,w2,w3) = state^.info.w
        gVal = state^.info.g
        v4 = v4' + gVal

data Alg23Info = Alg23Info
    { _uAlg23 :: (Int, Int)
    , _vAlg23 :: (Int, Int)
    , _wAlg23 :: (Int, Int) }
    deriving (Show, Eq, Ord)
makeLenses ''Alg23Info

-- Scans Right to Left, i.e., LSD
alg2Automaton :: Int -> [State Alg23Info]
alg2Automaton maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, digitAlphabet, digitAlphabet]
        withTransitions = makeTransitionsBy alphabet (^.info) alg2Dest states
        states = alg2States maxChar

alg2States :: Int -> [State Alg23Info]
alg2States maxChar = makeStates
    where
        -- Intentionally include 0 here, even though it's not a valid input, for the initial states
        fracAlphabet = [0..maxChar]
        digitAlphabet = [0..maxChar]
        initialInfo = Alg23Info (0,0) (0,0) (0,0)
        initialInfoList = [0,0,0,0,0,0]
        initial = State 0 (isFinalAlg2 initialInfoList) [] initialInfo
        makeStates = initial : zipWith go [1..] (concats (replicate 2 digitAlphabet ++ replicate 2 digitAlphabet ++ replicate 2 fracAlphabet))
            where
                go num l@[v1, v2, w1, w2, u1, u2] =
                    State num (isFinalAlg2 l) [] $ Alg23Info (u1,u2) (v1,v2) (w1,w2)

isFinalAlg2 :: [Int] -> Int
isFinalAlg2 [v1, v2, w1, w2, u1, u2]
    | v1 == w1 && v2 == w2 = 1
    | otherwise = 0

alg2Dest :: State Alg23Info -> [Int] -> Maybe Alg23Info
alg2Dest state [u1, v1, w1]
    | v1 < u1 && v2 == u2 && v3 > 0 =
        if v3-1 == w3 then Just $ Alg23Info (u1,u2) (v1+1,0) (w1,w2)
        else Nothing
    | v3 == w3 = Just $ Alg23Info (u1,u2) (v1,v2) (w1,w2)
    | otherwise = Nothing
    where
        (u2,u3) = state^.info.uAlg23
        (v2,v3) = state^.info.vAlg23
        (w2,w3) = state^.info.wAlg23

-- Scans Left to Right, i.e., MSD
alg3Automaton :: Int -> [State Alg23Info]
alg3Automaton maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, digitAlphabet, digitAlphabet]
        withTransitions = makeTransitionsBy alphabet (^.info) alg3Dest states
        states = alg3States maxChar

alg3States :: Int -> [State Alg23Info]
alg3States maxChar = makeStates
    where
        -- Intentionally include 0 here, even though it's not a valid input, for the initial states
        fracAlphabet = [0..maxChar]
        digitAlphabet = [0..maxChar]
        initialInfo = Alg23Info (0,0) (0,0) (0,0)
        initialInfoList = [0,0,0,0,0,0]
        initial = State 0 (isFinalAlg2 initialInfoList) [] initialInfo
        makeStates = initial : zipWith go [1..] (concats (replicate 2 digitAlphabet ++ replicate 2 digitAlphabet ++ replicate 2 fracAlphabet))
            where
                go num l@[v1, v2, w1, w2, u1, u2] =
                    State num (isFinalAlg3 l) [] $ Alg23Info (u1,u2) (v1,v2) (w1,w2)

isFinalAlg3 :: [Int] -> Int
isFinalAlg3 [v1, v2, w1, w2, u1, u2]
    | v1 == w1 && v2 == w2 = 1
    | otherwise = 0

alg3Dest :: State Alg23Info -> [Int] -> Maybe Alg23Info
alg3Dest state [u3, v3, w3]
    | v1 < u1 && v2 == u2 && v3 > 0 =
        if v1+1 == w1 then Just $ Alg23Info (u2,u3) (0,v3-1) (w2,w3)
        else Nothing
    | v1 == v2 = Just $ Alg23Info (u2,u3) (v2,v3) (w2,w3)
    | otherwise = Nothing
    where
        (u1,u2) = state^.info.uAlg23
        (v1,v2) = state^.info.vAlg23
        (w1,w2) = state^.info.wAlg23

