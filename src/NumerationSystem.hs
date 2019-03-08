{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module NumerationSystem where

import Control.Lens

import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map

import Automata
import Util

data RecogInfo = RecogInfo
    { _prev :: Int}
    deriving (Show, Eq, Ord)
makeLenses ''RecogInfo

generalRecogAutomata :: Int -> [State RecogInfo]
generalRecogAutomata maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        withTransitions = makeTransitionsBy alphabet (^.info) recogDest states
        states = generalRecogStates maxChar
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, digitAlphabet]

generalRecogStates :: Int -> [State RecogInfo]
generalRecogStates maxChar = initial : zipWith go [1..] stateVals
    where
        initial = State 0 1 Map.empty $ RecogInfo (-1)
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        stateVals = concats [digitAlphabet]
        go num [prev] = State num 1 Map.empty $ RecogInfo prev

recogDest :: State RecogInfo -> [Int] -> Maybe RecogInfo
recogDest state [nextFrac, nextDigit]
    -- Any continued fraction starting with 1 is not allowed to have a 1 in the first place
    | state^.num == 0 && nextDigit >= nextFrac = Nothing -- First digit must be strictly less than the fraction
    | nextDigit < nextFrac || (nextDigit == nextFrac && state^.info.prev == 0) = Just $ RecogInfo nextDigit
    | otherwise = Nothing

alg0Automaton :: Int -> [State ()]
alg0Automaton maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        digitAlphabet = [0..maxChar]
        sumAlphabet = [0..maxChar + maxChar + 1]
        alphabet = concats [digitAlphabet, digitAlphabet, sumAlphabet]
        withTransitions = makeTransitions alphabet alg0Dest alg0States

-- This automata always has just one state, anything that would transition it to an invalid state is just excluded
alg0States :: [State ()]
alg0States = [State 0 1 Map.empty ()]

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
alg1Automaton :: Int -> [State ()]
alg1Automaton maxChar = minimizeAutomata alphabet $ prune $ map (set info ()) withTransitions
    where
        fracAlphabet = [1..maxChar]
        -- These symbols come from directly summing the two strings, so the range is larger
        -- Additionally, we can add 1 more to any symbol during algorithm 1, so the range must be increased accordingly
        sumAlphabet = [0..maxChar + maxChar + 1]
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
        -- These symbols come from directly summing the two strings, so the range is larger
        -- Additionally, we can add 1 more to any symbol during algorithm 1, so the range must be increased accordingly
        sumAlphabet = [0..maxChar + maxChar + 1]
        initialInfo = Alg1Info (0,0,0) (0,0,0) (0,0,0) 0
        initialInfoList = [0,0,0,0,0,0,0,0,0,0]
        initial = State 0 (isFinalAlg1 initialInfoList) Map.empty initialInfo
        makeStates = initial : zipWith go [1..] (concats (replicate 3 sumAlphabet ++ replicate 3 digitAlphabet ++ replicate 3 fracAlphabet ++ [[0,1]]))
            where
                go num l@[v1, v2, v3, w1, w2, w3, u1, u2, u3, g] =
                    State num (isFinalAlg1 l) Map.empty $ Alg1Info (u1,u2,u3) (v1,v2,v3) (w1,w2,w3) g

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
    | v2 < u2 && v3 >= u3 && v3 <= 2*u3 && v4 > 0 && v1 == w1 = Just $ Alg1Info (u2,u3,u4) (v2+1,v3-u3,v4-1) (w2,w3,w4) 0
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
        initial = State 0 (isFinalAlg2 initialInfoList) Map.empty initialInfo
        makeStates = initial : zipWith go [1..] (concats (replicate 2 digitAlphabet ++ replicate 2 digitAlphabet ++ replicate 2 fracAlphabet))
            where
                go num l@[v1, v2, w1, w2, u1, u2] =
                    State num (isFinalAlg2 l) Map.empty $ Alg23Info (u1,u2) (v1,v2) (w1,w2)

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
        initial = State 0 (isFinalAlg2 initialInfoList) Map.empty initialInfo
        makeStates = initial : zipWith go [1..] (concats (replicate 2 digitAlphabet ++ replicate 2 digitAlphabet ++ replicate 2 fracAlphabet))
            where
                go num l@[v1, v2, w1, w2, u1, u2] =
                    State num (isFinalAlg3 l) Map.empty $ Alg23Info (u1,u2) (v1,v2) (w1,w2)

isFinalAlg3 :: [Int] -> Int
isFinalAlg3 [v1, v2, w1, w2, u1, u2]
    | v1 == w1 && v2 == w2 = 1
    | otherwise = 0

alg3Dest :: State Alg23Info -> [Int] -> Maybe Alg23Info
alg3Dest state [u3, v3, w3]
    | v1 < u1 && v2 == u2 && v3 > 0 =
        if v1+1 == w1 then Just $ Alg23Info (u2,u3) (0,v3-1) (w2,w3)
        else Nothing
    | v1 == w1 = Just $ Alg23Info (u2,u3) (v2,v3) (w2,w3)
    | otherwise = Nothing
    where
        (u1,u2) = state^.info.uAlg23
        (v1,v2) = state^.info.vAlg23
        (w1,w2) = state^.info.wAlg23

-- | This automaton takes the two numbers (CHECK THEY ARE BOTH VALID FIRST), and accepts if the
-- | First number is less than the second.
generalLt :: Int -> [State Bool]
generalLt maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        digitAlphabet = [0..maxChar]
        alphabet = concats [digitAlphabet, digitAlphabet]
        withTransitions = makeTransitionsBy alphabet (^.info) generalLtDest generalLtStates

generalLtStates :: [State Bool]
generalLtStates = [State 0 0 Map.empty False, State 1 1 Map.empty True]

generalLtDest :: State Bool -> [Int] -> Maybe Bool
generalLtDest state [x1, y1]
    | state^.info = Just True -- If we already found this number to be less than it, then we're done
    | x1 < y1 = Just True
    | x1 == y1 = Just False
    | otherwise = Nothing -- If x1 > y1, then x > y

generalEq :: Int -> [State ()]
generalEq maxChar = withTransitions
    where
        digitAlphabet = [0..maxChar]
        alphabet = concats [digitAlphabet, digitAlphabet]
        withTransitions = makeTransitionsBy alphabet (^.info) dest [State 0 1 Map.empty ()]
        dest _ [x,y]
            | x == y = Just ()
            | otherwise = Nothing

generalOne :: Int -> [State (Int, Bool)]
generalOne maxChar = minimizeAutomata alphabet $ prune withTransitions
    where
        fracAlphabet = [1..maxChar]
        digitAlphabet = [0..maxChar]
        alphabet = concats [fracAlphabet, digitAlphabet]
        withTransitions = makeTransitionsBy alphabet (^.info) generalOneDest states
        initial = State 0 0 Map.empty (0,False)
        states = initial : zipWith go [1..] [(prev, done) | prev <- fracAlphabet, done <- [False, True]]
        go num (prev, done) = State num (if done then 1 else 0) Map.empty (prev, done)

generalOneDest :: State (Int, Bool) -> [Int] -> Maybe (Int, Bool)
generalOneDest state [frac, digit]
    | done =
        -- All digits after the first 1 must be 0
        if digit == 0 then Just (prev, done)
        else Nothing
    | frac > 1 && digit == 1 && state^.num == 0 = Just (frac, True)
    | frac == 1 && digit == 0 && state^.num == 0 = Just (frac, False)
    | prev == 1 && digit == 1 = Just (frac, True)
    | otherwise = Nothing
    where (prev, done) = state^.info

generalZero :: [State ()]
generalZero = [State 0 1 (Map.fromList [([0],0)]) ()]

