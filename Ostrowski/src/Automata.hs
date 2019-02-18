{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Automata where

import Control.Lens

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Transition = Transition
    { _letter :: Int
    , _destState :: Int }
    deriving (Show, Eq)
makeLenses ''Transition

data State a = State
    { _num :: Int
    , _output :: Int
    , _transitions :: [Transition]
    , _info :: a }
    deriving (Show, Eq)
makeLenses ''State

stateWithNum :: [State a] -> Int -> State a
stateWithNum states n = fromJust $ find (\st -> st^.num == n) states

runAutomata :: [State a] -> [Int] -> ([Int], Int)
runAutomata states input = (input, last (fst (foldl run ([], head states) input)))
    where
        run (out, state) c = fromJust $ do
            t <- find (\t -> t^.letter == c) $ state^.transitions
            st <- find (\st -> st^.num == t^.destState) states
            pure (out ++ [st^.output], st)

automataOutput :: [State a] -> [[Int]] -> [Int]
automataOutput states = map (snd . runAutomata states)

reachableStates :: [State a] -> [State a]
reachableStates states@(state:_) = reachableStates' [] [state]
    where
        reachableStates' seen [] = map (stateWithNum states) $ sort seen
        reachableStates' seen (st:sts)
            | st^.num `elem` seen = reachableStates' seen sts
            | otherwise = reachableStates' (st^.num:seen) $ sts ++ map ((stateWithNum states) . (^.destState)) (st^.transitions)

unreachableStates :: Eq a => [State a] -> [State a]
unreachableStates states = states \\ reachableStates states

renumberStates :: [State a] -> [State a]
renumberStates states = map (renumber newNumbers) states
    where
        newNumbers = Map.fromList $ zip (map (^.num) states) [0..]

renumber :: Map Int Int -> State a -> State a
renumber newNumbers st =
    set transitions newTransitions $
    set num (fromJust (Map.lookup (st^.num) newNumbers)) st
    where
        newTransitions = map (renumberTransition newNumbers) $ st^.transitions

renumberTransition :: Map Int Int -> Transition -> Transition
renumberTransition newNumbers t = set destState (fromJust (Map.lookup (t^.destState) newNumbers)) t

prune :: [State a] -> [State a]
prune = renumberStates . reachableStates

class WalnutOutput a where
    walnutStr :: a -> String

instance WalnutOutput a => WalnutOutput [a] where
    walnutStr = intercalate "\n" . map walnutStr

instance WalnutOutput Transition where
    walnutStr trans = show (trans^.letter) ++ " -> " ++ show (trans^.destState)

instance WalnutOutput (State a) where
    walnutStr st = show (st^.num) ++ " " ++ show (st^.output) ++ "\n" ++
                    walnutStr (st^.transitions)

walnutOutput numSys states = numSys ++ "\n" ++ walnutStr states

