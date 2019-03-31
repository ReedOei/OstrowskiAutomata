{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module BaseNAutomata where

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
    , _len :: Int
    , _maxLen :: Int
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
        alphabet = map (\range -> [0..maximum range]) inAlphabet
        symbols = concats alphabet

        newAlphabet = concats $ replicate (length alphabet) [0..base - 1]

        maxVal = maximum (map length alphabet) + 1
        maxLen = ceiling $ log (fromIntegral maxVal) / log (fromIntegral base)

        states = zipWith (\num f -> f num) [0..] $ concatMap (makeBaseNState base maxLen symbols) origStates
        withTransitions = makeTransitionsBy newAlphabet getInfo toBaseNDest states

getInfo state = (bi^.vals, bi^.len, bi^.maxLen, bi^.base, bi^.origNum)
    where
        bi = state^.info

makeBaseNState :: Int -> Int -> [[Int]] -> State a -> [Int -> State BaseInfo]
makeBaseNState base maxLen symbols origState = [go symbol len | symbol <- symbols, len <- [0..maxLen - 1]]
    where
        go symbol len i = State i (origState^.output) Map.empty $ BaseInfo symbol len maxLen base (origState^.num) $ origState^.transitions

toBaseNDest :: State BaseInfo -> [Int] -> Maybe ([Int], Int, Int, Int, Int)
toBaseNDest state letter
    | bi^.len + 1 < bi^.maxLen = Just (newVals, bi^.len + 1, bi^.maxLen, bi^.base, bi^.origNum)
    | otherwise = (\dest -> (replicate (length newVals) 0, 0, bi^.maxLen, bi^.base, dest)) <$> Map.lookup newVals (bi^.origTrans)
    where
        bi = state^.info
        newVals = zipWith (\new prev -> prev + new*(bi^.base)^(bi^.len)) letter $ bi^.vals

