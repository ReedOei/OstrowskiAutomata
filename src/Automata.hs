{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Automata where

import Control.Lens
import Control.Monad
import qualified Control.Monad.State.Strict as St

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.List
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

import System.IO
import System.IO.Unsafe

import Util

data State a = State
    { _num :: Int
    , _output :: Int
    , _transitions :: Map [Int] Int
    , _info :: a }
    deriving (Show, Eq)
makeLenses ''State

replace :: Eq a => a -> a -> [a] -> [a]
replace search rep = map (\x -> if x == search then rep else x)

stateWithNum :: [State a] -> Int -> State a
stateWithNum states n = fromJust $ find (\st -> st^.num == n) states

alphabetOf :: [State a] -> [[Int]]
alphabetOf = nub . concatMap (Map.keys . (^.transitions))

makeTransitionsBy :: Ord b =>
                     [[Int]] ->
                     (State a -> b) -> -- Function that converts states into information type (to create state map)
                     (State a -> [Int] -> Maybe b) -> -- Generate the destination state
                     [State a] ->  -- Input states
                     [State a] -- States with transitions
makeTransitionsBy alphabet keyMapper dest states = map makeTransition states
    where
        stateMap = Map.fromList $ map (\s -> (keyMapper s, s^.num)) states
        makeTransition state = set transitions newTrans state
            where
                newTrans = Map.fromList [ (symbol, fullState) | symbol <- alphabet,
                                  fullState <- maybeToList $ dest state symbol >>= (`Map.lookup` stateMap) ]

-- Simpler variant for when you just want equality for output/info (must define ordering for state info type)
makeTransitions alphabet = makeTransitionsBy alphabet keyMapper
    where
        keyMapper state = (state^.output, state^.info)

transitionNum :: State a -> [Int] -> Maybe Int
transitionNum state c = Map.lookup c $ state^.transitions

transition :: Map Int (State a) -> State a -> [Int] -> Maybe (State a)
transition stateMap state c = do
    num <- transitionNum state c
    Map.lookup num stateMap

runAutomata :: [State a] -> [[Int]] -> Maybe (([[Int]], Int), ([State a], State a))
runAutomata states input =
    case foldM run (([], []), head states) input of
        Nothing -> Nothing
        Just ((output, sts), finalSt) -> Just ((input, last output), (sts, finalSt))
    where
        stateMap = Map.fromList $ map (\state -> (state^.num, state)) states

        run ((out, stList), state) c =
            case transition stateMap state c of
                Nothing -> Nothing
                Just st -> Just ((out ++ [st^.output], stList ++ [st]), st)

automataOutput :: [State a] -> [[[Int]]] -> [Int]
automataOutput states = map (\input -> fromMaybe (-1) (snd . fst <$> runAutomata states input))

outputAutomataToAcceptAutomata :: Eq a => Int -> [State a] -> [State a]
outputAutomataToAcceptAutomata acceptedOutput states =
    minimizeAutomata alphabet $ prune $ map (over output changeOutput) states
    where
        alphabet = nub $ sort $ concatMap (Map.keys . (^.transitions)) states
        changeOutput outputVal
            | outputVal == acceptedOutput = 1
            | otherwise = 0

reachableStates :: [State a] -> [State a]
reachableStates states@(state:_) = reachableStates' Set.empty [state]
    where
        stateArr = Array.array (0, length states - 1) $ map (\state -> (state^.num, state)) states

        reachableStates' seen [] = map (stateArr !) $ sort $ Set.toList seen
        reachableStates' seen (st:sts)
            | Set.member (st^.num) seen = reachableStates' seen sts
            | otherwise = reachableStates' (Set.insert (st^.num) seen) $ map (stateArr !) (Map.elems (st^.transitions)) ++ sts

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
        newTransitions = Map.map (renumberTransition newNumbers) $ st^.transitions

-- Using the specified renumbering, updates all transitions
renumberTransition :: Map Int Int -> Int -> Int
renumberTransition newNumbers t = fromJust $ Map.lookup t newNumbers

-- Removes all unreachable states from an automata
prune :: [State a] -> [State a]
prune = renumberStates . reachableStates

minimizeAutomata :: Eq a => [[Int]] -> [State a] -> [State a]
minimizeAutomata alphabet states = prune $ map (renumber newNumbers) states
    where
        newNumbers = foldl Map.union Map.empty $ map newNumber finalPartitions
        newNumber partition = Map.fromList $ zip nums $ repeat $ minimum nums
            where nums = map (^.num) partition
        finalPartitions = untilNoChange (distinguish alphabet) partitions
        partitions = groupBy (\a b -> a^.output == b^.output) $ sortBy (comparing (^.output)) states

-- For each letter in the alphabet, we can distinguish two different states x and y if that letter a from x goes to a state in a different partition than it does from y
-- iterate distinguish until it stops changing.
distinguish alphabet partitions = unsafePerformIO $ do
    hPutStrLn stderr $ "Minimizing, have " ++ show (length partitions) ++ " partitions, " ++ show stateNum ++ " states."
    pure $ concatMap distinguish' partitions
    where
        stateNum = length (concat partitions)
        -- Maps state numbers to their partition index
        partitionArr = Array.array (0, stateNum - 1) $ concat $ zipWith (\i states -> map (\s -> (s^.num,i)) states) [0..] partitions

        doLookup maybeNum = fromMaybe (-1) $ do
            num <- maybeNum
            pure $ partitionArr ! num

        -- Input is list of (key,val) pairs, returns equivalence classes by comparing first element of the part
        repartition = groupBy ((==) `on` fst) . sortBy (comparing fst)

        -- Splits the partition into (potentially) several partitions, each of which is a distinguishable group
        distinguish' partition = map (map snd) $ repartition $ St.evalState destStates Map.empty
            where
                destPartition :: State a -> St.State (Map [Int] Int) (Int, State a)
                destPartition x = do
                    let dests = map (doLookup . transitionNum x) alphabet

                    m <- St.get
                    case Map.lookup dests m of
                        Nothing -> do
                            St.modify $ Map.insert dests $ Map.size m
                            pure (Map.size m, x)
                        Just i -> pure (i, x)

                destStates = mapM destPartition partition

-- | Lists all inputs that give the specified output in the automata given
findInputs :: Int -> [State a] -> [[[Int]]]
findInputs targetOutput states@(start:_) = search Set.empty [([], start)]
    where
        stateMap = Map.fromList $ map (\state -> (state^.num, state)) states

        findState n = maybeToList $ Map.lookup n stateMap

        search _ [] = []
        search seen ((path,st):sts)
            | Set.member (st^.num) seen = search seen sts
            | st^.output == targetOutput = path : search newSeen newSts
            | otherwise = search newSeen newSts
            where
                newSeen = Set.insert (st^.num) seen
                newStates = concatMap (\(symbol,dest) -> map (path ++ [symbol],) $ findState dest) $ Map.toList $ st^.transitions
                newSts = sts ++ newStates

class WalnutOutput a where
    walnutStr :: a -> String

instance WalnutOutput a => WalnutOutput [a] where
    walnutStr = intercalate "\n" . map walnutStr

instance WalnutOutput (State a) where
    walnutStr st = show (st^.num) ++ " " ++ show (st^.output) ++ "\n" ++ intercalate "\n" (map go (Map.toList (st^.transitions)))
        where
            go (letter, destState) = unwords (map show letter) ++ " -> " ++ show destState

walnutOutput numSys states = numSys ++ "\n" ++ walnutStr states

