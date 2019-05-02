{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module BaseNAutomataAlt where

import Control.Lens
import qualified Control.Monad.State.Strict as St

import Data.List
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

import Automata
import Util

data BaseInfo = BaseInfo
    { _vals :: [Int]
    , _done :: [Bool]
    , _base :: Int
    , _origNum :: Int
    , _origTrans :: Map [Int] Int }
    deriving (Show, Eq)
makeLenses ''BaseInfo

-- toBaseN :: Int -> [[Int]] -> [State a] -> [State a]
toBaseN base inAlphabet origStates = minimizeAutomata newAlphabet $ prune withTransitions
-- toBaseN base inAlphabet origStates = withTransitions
    where
        -- Normalize the alphabet a little (i.e., make things like [1..4] into [0..4])
        -- This doesn't change the behavior: if we get 0 for some symbol that should be in the range [1..4],
        -- it simply won't transition anywhere--it'll be rejected
        -- Note: We also add another symbol to signal that we're done in some spot
        alphabet = map (\range -> [0..maximum range]) inAlphabet
        symbols = concats alphabet

        newAlphabet = concats $ replicate (length alphabet) [0..base]

        states = zipWith (\num f -> f num) [0..] $ concatMap (makeBaseNState base symbols) origStates
        withTransitions = makeTransitionsBy newAlphabet getInfo toBaseNDest states

getInfo state = (bi^.vals, bi^.done, bi^.base, bi^.origNum)
    where bi = state^.info

makeBaseNState :: Int -> [[Int]] -> State a -> [Int -> State BaseInfo]
makeBaseNState base symbols origState = [go symbol done | symbol <- symbols, done <- concats (replicate l [False,True])]
    where
        l = length (head symbols)
        go symbol done i = State i (origState^.output) Map.empty $ BaseInfo symbol done base (origState^.num) $ origState^.transitions

toBaseNDest :: State BaseInfo -> [Int] -> Maybe ([Int], [Bool], Int, Int)
toBaseNDest state letter
    | and newDone =
        case Map.lookup newVals $ bi^.origTrans of
            Nothing -> Nothing
            Just dest ->
                let l = length newVals
                in Just (replicate l 0, replicate l False, bi^.base, dest)
    | otherwise = Just (newVals, newDone, bi^.base, bi^.origNum)
    where
        bi = state^.info

        newDone = zipWith updateDone letter $ bi^.done
        updateDone val isDone
            | isDone = isDone
            | otherwise = bi^.base == val -- e.g., if we get 2 while converting to binary, then we're done

        newVals = zipWith update letter $ zip (bi^.vals) newDone
        update new (prev,isDone)
            | isDone = prev
            | otherwise = 2*prev + new

