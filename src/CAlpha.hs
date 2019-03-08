{-# LANGUAGE TemplateHaskell #-}

module CAlpha where

import Control.Lens

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Automata

data StateInfo = StateInfo
    { _isEven :: Bool
    , _pastStart :: Bool }
    deriving (Show, Eq)
makeLenses ''StateInfo

genAutomata alphabet =
    minimizeAutomata alphabet $ prune $
    genTransitions alphabet $ genStates alphabet

genStates alphabet = zipWith go [0..] [(isEven, pastStart) | isEven <- [True, False], pastStart <- [False, True]]
    where
        go num (isEven, pastStart) = State num (if isEven then 0 else 1) Map.empty $ StateInfo isEven pastStart

transitionDest :: State StateInfo -> [Int] -> State StateInfo
transitionDest st letter =
    over info (set isEven newEven . set pastStart newPastStart) st
    where
        i = st^.info
        newEven
            | i^.pastStart = i^.isEven
            | letter == [0] = not $ i^.isEven
            | otherwise = i^.isEven
        newPastStart = i^.pastStart || letter /= [0]

genTransitions alphabet states = map (transitionsForState alphabet states) states

transitionsForState alphabet destStates state = set transitions stTransitions state
    where
        stTransitions = Map.fromList $ map go alphabet
        go letter = (letter, newSt^.num)
            where newSt = findDest destStates $ transitionDest state letter

findDest states state =
    case find (stateMatching state) states of
        Just st -> st

stateMatching st check =
    let i = st^.info
        ci = check^.info
    in i^.isEven == ci^.isEven &&
       i^.pastStart == ci^.pastStart

