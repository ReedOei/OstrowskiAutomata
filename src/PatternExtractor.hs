{-# LANGUAGE TupleSections #-}

module PatternExtractor where

import Control.Lens

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord hiding (Ordering(..))

import Prelude hiding (EQ,LT,GT)

import Automata
import AutomataParser (parseAutomata, readUtf16File)
import Util

data Rel = LTE | GTE | EQ | LT | GT | Any
    deriving (Show, Eq, Ord)

type Orderings = [[[Rel]]]

orderings :: Ord a => [a] -> Orderings
orderings xs = [[makeComp x y | (j,y) <- drop (i + 1) $ zip [0..] xs] | (i,x) <- init $ zip [0..] xs]
    where
        makeComp x y
            | x == y = [EQ]
            | x < y = [LT]
            | x > y = [GT]

partitions m [] = [m]
partitions m ((o,target):orderings) = concatMap try choices
    where
        choices = concats $ map concats o

        try choice = partitions (Map.insertWith (+) (choice, target) 1 m) orderings

best :: (Ord k, Num a) => [Map k a] -> [k]
best = Map.keys . minimumBy (comparing Map.size)

readAutomaton :: FilePath -> IO [State ()]
readAutomaton path = snd . parseAutomata <$> readUtf16File path

automatonPatterns :: FilePath -> IO ()
automatonPatterns path = do
    statePatterns <- map statePattern <$> readAutomaton path
    mapM_ prettyPrint statePatterns
    where
        prettyPrint (num, out, trans) = do
            putStrLn $ show num ++ " " ++ show out
            mapM_ prettyTrans trans
        prettyTrans (symbol, dest) = putStrLn $ unwords (map show symbol) ++ " -> " ++ show dest

partitionBy f = groupBy (\a b -> f a == f b) . sortBy (comparing f)

statePattern state = (state^.num, state^.output, concat $ map (groupPatterns . best . partitions Map.empty) $ partitionBy snd ords)
    where
        ords = map (over _1 orderings) $ Map.toList $ state^.transitions

unify pat1 pat2 = sequence $ zipWith (\a b -> sequence $ zipWith unify' a b) pat1 pat2
    where
        unify' EQ GTE = Just GTE
        unify' EQ LTE = Just LTE
        unify' GT GTE = Just GTE
        unify' LT LTE = Just LTE
        unify' GTE EQ = Just GTE
        unify' LTE EQ = Just LTE
        unify' GTE GT = Just GTE
        unify' LTE LT = Just LTE
        unify' LT EQ = Just LTE
        unify' GT EQ = Just GTE
        unify' EQ LT = Just LTE
        unify' EQ GT = Just GTE
        unify' LTE GT = Just Any
        unify' GTE LT = Just Any
        unify' GT LTE = Just Any
        unify' LT GTE = Just Any
        unify' Any _ = Just Any
        unify' _ Any = Just Any
        unify' x y
            | x == y = Just x
            | otherwise = Nothing

unifyInto :: (a -> a -> Maybe a) -> [a] -> a -> [a]
unifyInto _ [] x     = [x]
unifyInto f (y:ys) x =
    case f x y of
        Nothing -> y : unifyInto f ys x
        Just new -> new : ys

unifyFold :: (a -> a -> Maybe a) -> [a] -> [a]
unifyFold f = foldl (unifyInto f) []

groupPatterns patterns@((_,dest):_) = map (,dest) $ untilNoChange (unifyFold unify) patRel
    where
        patRel = map fst patterns

