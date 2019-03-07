{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Automata where

import Control.Lens

import Data.List
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

data Transition = Transition
    { _letter :: [Int]
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

replace :: Eq a => a -> a -> [a] -> [a]
replace search rep = map (\x -> if x == search then rep else x)

stateWithNum :: [State a] -> Int -> State a
stateWithNum states n = fromJust $ find (\st -> st^.num == n) states

alphabetOf :: [State a] -> [[Int]]
alphabetOf = nub . concatMap (map (^.letter) . (^.transitions))

makeTransitionsBy :: Ord b =>
                     [[Int]] ->
                     (State a -> b) -> -- Function that converts states into information type (to create state map)
                     (State a -> [Int] -> Maybe b) -> -- Generate the destination state
                     [State a] ->  -- Input states
                     [State a] -- States with transitions
makeTransitionsBy alphabet keyMapper dest states = map makeTransition states
    where
        stateMap = Map.fromList $ map (\s -> (keyMapper s, s)) states
        makeTransition state = set transitions newTrans state
            where
                newTrans = [ Transition symbol $ fullState^.num | symbol <- alphabet,
                                  fullState <- maybeToList $ dest state symbol >>= (`Map.lookup` stateMap) ]

-- Simpler variant for when you just want equality for output/info (must define ordering for state info type)
makeTransitions alphabet = makeTransitionsBy alphabet keyMapper
    where
        keyMapper state = (state^.output, state^.info)

transition :: Map Int (State a) -> State a -> [Int] -> Maybe (State a)
transition stateMap state c = do
    t <- find (\t -> t^.letter == c) $ state^.transitions
    Map.lookup (t^.destState) stateMap

runAutomata :: [State a] -> [[Int]] -> (([[Int]], Int), ([State a], State a))
runAutomata states input =
    let ((output, sts), finalSt) = foldl run (([], []), head states) input
    in ((input, last output), (sts, finalSt))
    where
        stateMap = Map.fromList $ map (\state -> (state^.num, state)) states

        run ((out, stList), state) c =
            let st = fromJust $ transition stateMap state c
            in ((out ++ [st^.output], stList ++ [st]), st)

automataOutput :: [State a] -> [[[Int]]] -> [Int]
automataOutput states = map (snd . fst . runAutomata states)

outputAutomataToAcceptAutomata :: Eq a => Int -> [State a] -> [State a]
outputAutomataToAcceptAutomata acceptedOutput states =
    minimizeAutomata alphabet $ prune $ map (over output changeOutput) states
    where
        alphabet = nub $ sort $ concatMap (map (^.letter) . (^.transitions)) states
        changeOutput outputVal
            | outputVal == acceptedOutput = 1
            | otherwise = 0

reachableStates :: [State a] -> [State a]
reachableStates states@(state:_) = reachableStates' [] [state]
    where
        stateMap = Map.fromList $ map (\state -> (state^.num, state)) states

        findState n = maybeToList $ Map.lookup n stateMap

        reachableStates' seen [] = concatMap findState $ sort seen
        reachableStates' seen (st:sts)
            | st^.num `elem` seen = reachableStates' seen sts
            | otherwise = reachableStates' (st^.num:seen) $ concatMap (findState . (^.destState)) (st^.transitions) ++ sts

unreachableStates :: Eq a => [State a] -> [State a]
unreachableStates states = states \\ reachableStates states

renumberStates :: [State a] -> [State a]
renumberStates states = map (renumber newNumbers) states
    where
        newNumbers = Map.fromList $ zip (map (^.num) states) [0..]

-- Renumbers all states and transitions according to the renumbering map given.
renumber :: Map Int Int -> State a -> State a
renumber newNumbers st =
    set transitions newTransitions $
    set num (fromJust (Map.lookup (st^.num) newNumbers)) st
    where
        newTransitions = map (renumberTransition newNumbers) $ st^.transitions

-- Using the specified renumbering, updates all transitions
renumberTransition :: Map Int Int -> Transition -> Transition
renumberTransition newNumbers t = set destState (fromJust (Map.lookup (t^.destState) newNumbers)) t

-- Removes all unreachable states from an automata
prune :: [State a] -> [State a]
prune = renumberStates . reachableStates

untilNoChange :: Eq a => (a -> a) -> a -> a
untilNoChange f x
    | newVal == x = x
    | otherwise = untilNoChange f newVal
    where newVal = f x

-- | Finds all reachable states, given a list of inputs.
--   Can be useful if your input language is not just Sigma*.
optimize :: Eq a => [State a] -> [[[Int]]] -> [State a]
optimize states = nub . concatMap (fst . snd . runAutomata states)

minimizeAutomata :: Eq a => [[Int]] -> [State a] -> [State a]
minimizeAutomata alphabet states = prune $ map (renumber newNumbers) states
    where
        stateMap = Map.fromList $ map (\state -> (state^.num, state)) states
        newNumbers = foldl Map.union Map.empty $ map newNumber finalPartitions
        newNumber partition = Map.fromList $ zip nums $ repeat $ minimum nums
            where nums = map (^.num) partition
        finalPartitions = untilNoChange (distinguish alphabet stateMap) partitions
        partitions = groupBy (\a b -> a^.output == b^.output) $ sortBy (comparing (^.output)) states

-- For each letter in the alphabet, we can distinguish two different states x and y if that letter a from x goes to a state in a different partition than it does from y
-- iterate distinguish until it stops changing.
distinguish alphabet stateMap partitions = sortBy (comparing (map (^.num))) $ concatMap distinguish' partitions
    where
        -- Splits the partition into (potentially) several partitions, each of which is a distinguishable group
        distinguish' partition = map (map fst) $ groupBy ((==) `on` snd) $ sortBy (comparing snd) destStates
            where
                destPartition x = (x, [i | letter <- alphabet, let maybeSt = transition stateMap x letter,
                                           let i = if isJust maybeSt then
                                                      fromMaybe (-1) (findIndex (fromJust maybeSt `elem`) partitions)
                                                   else (-1)])
                destStates = map destPartition partition

class WalnutOutput a where
    walnutStr :: a -> String

instance WalnutOutput a => WalnutOutput [a] where
    walnutStr = intercalate "\n" . map walnutStr

instance WalnutOutput Transition where
    walnutStr trans = unwords (map show (trans^.letter)) ++ " -> " ++ show (trans^.destState)

instance WalnutOutput (State a) where
    walnutStr st = show (st^.num) ++ " " ++ show (st^.output) ++ "\n" ++
                    walnutStr (st^.transitions)

walnutOutput numSys states = numSys ++ "\n" ++ walnutStr states

