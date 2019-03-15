{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module PatternExtractor where

import Control.Lens
import Control.Monad

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord hiding (Ordering(..))

import Prelude hiding (EQ,LT,GT)

import Automata
import AutomataParser (parseAutomata, readUtf16File)
import Util

data Rel = LTE | GTE | EQ | LT | GT | Any
    deriving (Show, Eq, Ord)

type Orderings = [[[Rel]]]

orderings :: Ord a => [a] -> Orderings
orderings xs = [[makeComp x y | y <- drop (i + 1) xs] | (i,x) <- init $ zip [0..] xs]
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

data Ranges a b = Ranges
    { _ranges :: [(a,a)]
    , _val :: b }
    deriving (Show, Eq, Ord)
makeLenses ''Ranges

automatonRanges :: State () -> [Ranges Int Int]
automatonRanges state = map makeRange $ Map.toList $ state^.transitions
    where
        makeRange (symbol, dest) = Ranges (map (\a -> (a,a)) symbol) dest

simplifyRanges state = sortBy (comparing (^.val)) $ unifyFold (joinRanges allRanges) allRanges
    where
        allRanges = automatonRanges state

printRanges state = do
    putStrLn $ show (state^.num) ++ " " ++ show (state^.output)
    mapM_ printTrans $ simplifyRanges state
    where
        printTrans rs = do
            -- putStrLn $ unwords (map show (rs^.ranges)) ++ " -> " ++ show (rs^.val)
            putStrLn $ show (rs^.ranges) ++ " -> " ++ show (rs^.val)

inRanges :: Ord a => Ranges a b -> [a] -> Bool
inRanges rs vals = and $ zipWith inRange vals $ rs^.ranges
    where
        inRange v (a,b) = a <= v && v <= b

generateRanges :: Enum a => Ranges a b -> [([a],b)]
generateRanges rs = map (,rs^.val) $ concats $ map go $ rs^.ranges
    where
        go (a,b) = [a..b]

joinRange :: (Enum a, Ord a) => (a,a) -> (a,a) -> Maybe (a,a)
joinRange (a,b) (c,d)
    | a <= c && c <= b && b <= d = Just (a,d)
    | c <= a && a <= d && d <= b = Just (c,b)
    | a <= c && d <= b = Just (a,b)
    | c <= a && b <= d = Just (c,d)
    | succ b == c = Just (a,d)
    | succ d == a = Just (c,b)
    | otherwise = Nothing

overlapRange :: (Enum a, Ord a) => (a,a) -> (a,a) -> Maybe (a,a)
overlapRange (a,b) (c,d)
    | a <= c && c <= b && b <= d = Just (a,d)
    | c <= a && a <= d && d <= b = Just (c,b)
    | a <= c && d <= b = Just (a,b)
    | c <= a && b <= d = Just (c,d)
    | otherwise = Nothing

tryJoin r1 r2 = zipWithM joinRange (r1^.ranges) (r2^.ranges)

joinRanges :: (Enum a, Ord a, Eq b) => [Ranges a b] -> Ranges a b -> Ranges a b -> Maybe (Ranges a b)
joinRanges allRanges r1 r2
    | r1^.val == r2^.val = do
        newRanges <- tryJoin r1 r2
        -- Make sure the newly joined ranges don't overlap with any of the ranges from other categories
        guard $ null $ mapMaybe (tryOverlap (r1^.val) newRanges) allRanges
        pure $ Ranges newRanges $ r1^.val
    | otherwise = Nothing
    where
        tryOverlap v newRanges r
            | v /= r^.val = zipWithM overlapRange (r^.ranges) newRanges
            | otherwise = Nothing

automatonPatterns :: FilePath -> IO ()
automatonPatterns path = do
    statePatterns <- map statePattern <$> readAutomaton path
    mapM_ prettyPrint statePatterns
    where
        prettyPrint (num, out, trans) = do
            putStrLn $ show num ++ " " ++ show out
            mapM_ prettyTrans trans
        prettyTrans (symbol, dest) = putStrLn $ unwords (map show symbol) ++ " -> " ++ show dest

genPatternAutomaton :: [[Int]] -> FilePath -> IO (String, [State ()])
genPatternAutomaton alphabet path = do
    statePatterns <- map statePattern <$> readAutomaton path
    let numSys = makeAlphabetStr alphabet
    let symbols = concats alphabet
    pure (numSys, map (makePatternStates symbols) statePatterns)

makePatternStates symbols (num, out, trans) = State num out transMap ()
    where
        transMap = Map.fromList $ concatMap (makeTrans symbols) trans

makeTrans symbols (pat,dest) = map (,dest) $ filter (matches pat) symbols

matches pat symbol = and $ zipWith matches' pat [(x, drop (i+1) symbol) | (i,x) <- init $ zip [0..] symbol]
    where
        matches' rels (x, xs) = and $ zipWith check rels xs
            where
                check GT y  = x > y
                check EQ y  = x == y
                check LT y  = x < y
                check LTE y = x <= y
                check GTE y = x >= y
                check Any _ = True

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

