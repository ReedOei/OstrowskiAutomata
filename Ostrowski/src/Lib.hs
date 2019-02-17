{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Lib where

import Control.Lens

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord

import System.Environment

import Debug.Trace

p _ (-2) = 0
p _ (-1) = 1
p (d:ds) n = d * p ds (n - 1) + p (tail ds) (n - 2)

q _ (-2) = 1
q _ (-1) = 0
q (d:ds) n = d * q ds (n - 1) + q (tail ds) (n - 2)

dif ds n = q ds n - p ds n

repeatsEvery :: Eq a => Int -> [a] -> Bool
repeatsEvery _      []     = True
repeatsEvery period (x:xs) =
    case drop (period - 1) xs of
        []     -> True
        (y:ys) -> x == y && repeatsEvery period ys

isPeriodicWith :: Eq a => Int -> [a] -> Bool
isPeriodicWith period = all (repeatsEvery period) . take period . tails

periodic :: Eq a => [a] -> Bool
periodic [] = False
periodic xs = any ($ xs) $ map isPeriodicWith $ zipWith const [1..] $ tail xs

chunk _ [] = []
chunk n xs =
    let (c, rest) = splitAt n xs
    in c : chunk n rest

-- isPeriodic :: Eq a => [a] -> Bool
-- isPeriodic = any periodic . tails

t xs = find (\periodicPart -> all (== periodicPart) (chunk (length periodicPart) xs)) $ tail $ inits xs

takeUntil f = takeUntil' f []
    where
        takeUntil' _ prev [] = prev
        takeUntil' f prev (x:xs)
            | f (prev ++ [x]) = prev ++ [x]
            | otherwise       = takeUntil' f (prev ++ [x]) xs

-- computePeriod ds m = takeUntil isPeriodic $ map ((`mod` m) . dif ds) [0..]
computePeriod ds m = map (\i -> (dif (reverse (take (i + 1) ds)) i ) `mod` m) [0..]

data Transition = Transition
    { _letter :: Int
    , _destState :: Int }
    deriving (Show, Eq)
makeLenses ''Transition

data State = State
    { _num :: Int
    , _output :: Int
    , _zDiff :: Int
    , _oDiff :: Int
    , _seqNum :: Int
    , _seqLen :: Int
    , _transitions :: [Transition]
    , _isEven :: Bool
    , _pastStart :: Bool
    , _zMod :: Int
    , _oMod :: Int }
    deriving (Show, Eq)
makeLenses ''State

-- Transitions for the repeated part
transitionDest st letter n m =
    set isEven newEven $
    set zMod newZMod $
    set pastStart newPastStart $
    set seqNum newSeqNum $
    set oMod newOMod st
    where
        newEven
            | st^.pastStart = st^.isEven
            | letter == 0 = not $ st^.isEven
            | otherwise = st^.isEven
        newPastStart = st^.pastStart || letter /= 0
        newSeqNum = (st^.seqNum + 1) `mod` (st^.seqLen)
        newZMod = (letter * st^.zDiff + st^.zMod) `mod` n
        newOMod = (letter * st^.oDiff + st^.oMod) `mod` m

type Periodic = ([Int], [Int])

genAutomata alphabet zPeriod oPeriod zRep oRep =
    -- [genStartStartTrans n m alphabet (nonRepStates ++ repStates) startState] ++
    genNonRepTrans n m alphabet nonRepStates repStates ++
    genTransitions n m alphabet repStates repStates
    where
        n = genericLength zRep
        m = genericLength oRep
        (nonRepeat, repeat) = makePeriods zPeriod oPeriod
        -- startState = genStartState (nonRepeat ++ repeat) zRep oRep
        (nonRepStates, repStates) = genStates nonRepeat repeat zRep oRep

rotate :: Int -> [a] -> [a]
rotate n xs = zipWith (flip const) xs $ drop n $ cycle xs

makePeriods (zNonRep, zRep) (oNonRep, oRep) = (nonRepeat, rep)
    where
        nonRepeat
            | length zNonRep > length oNonRep = zip zNonRep $ oNonRep ++ concat (repeat oRep)
            | otherwise = zip (zNonRep ++ concat (repeat zRep)) oNonRep
        newZRep = rotate (length nonRepeat - length zNonRep) zRep
        newORep = rotate (length nonRepeat - length oNonRep) oRep
        rep = zipUntilRepeat (max (length newZRep) (length newORep)) (cycle newZRep) $ cycle newORep

zipUntilRepeat minL = zipUntilRepeat' []
    where
        zipUntilRepeat' acc _ [] = acc
        zipUntilRepeat' acc [] _ = acc
        zipUntilRepeat' acc (x:xs) (y:ys)
            | (x, y) `elem` acc && length acc >= minL = acc
            | otherwise = zipUntilRepeat' (acc ++ [(x,y)]) xs ys

genStartStartTrans n m alphabet destStates state =
    set transitions stTransitions state
    where
        stTransitions = map go alphabet
        go letter = Transition letter $ newSt^.num
            where newSt = findDest destStates $ set zMod 0 $ set oMod 0 $ transitionDest state letter n m

genNonRepTrans :: Int -> Int -> [Int] -> [State] -> [State] -> [State]
genNonRepTrans n m alphabet nonRepStates repStates =
    zipWith (\st dest -> transitionsForState n m alphabet dest st) nonRepStates $ tail $ tails $ nonRepStates ++ repStates

-- genStartState diffs zRep oRep = State 0 0 (head diffs) [] True False 0 0

genTransitions n m alphabet destStates = map (transitionsForState n m alphabet destStates)

transitionsForState n m alphabet destStates state =
    set transitions stTransitions state
    where
        stTransitions = map go alphabet
        go letter = Transition letter $ newSt^.num
            where newSt = findDest destStates $ transitionDest state letter n m

findDest states state =
    case find (stateMatching state) states of
        Just st -> st

stateMatching st check =
    st^.isEven == check^.isEven &&
    st^.zMod == check^.zMod &&
    st^.pastStart == check^.pastStart &&
    st^.seqNum == check^.seqNum &&
    st^.oMod == check^.oMod

genStates nonRepeat repeat zRep oRep = (nonRepStates, repStates)
    where
        n = genericLength zRep
        m = genericLength oRep
        (nonRepStates, k) = genStatesWithDiffs 0 nonRepeat zRep oRep
        (repStates, _) = genStatesWithDiffs k repeat zRep oRep

genStatesWithDiffs startNum diffs zRep oRep = (zipWith go [startNum..] stateInfo, endNum)
    where
        endNum = startNum + genericLength stateInfo
        stateInfo = [ (seqNum, diff, isEven, pastStart, zMod, oMod) | (seqNum, diff) <- zip [0..] diffs,
                                                                      isEven <- [True, False],
                                                                      pastStart <- [False, True],
                                                                      zMod <- [0..n - 1],
                                                                      oMod <- [0..m - 1]]
        n = genericLength zRep
        m = genericLength oRep
        go num (seqNum, (zDiff, oDiff), isEven, pastStart, zMod, oMod) = State num out zDiff oDiff seqNum (length diffs) [] isEven pastStart zMod oMod
            -- Need to shift by one here because the formula gives us the number including the current position
            where out = if isEven then zRep !! ((zMod - 1) `mod` n) else oRep !! ((oMod - 1) `mod` m)

ostrowski :: (Eq a, Num a) => [a] -> [a]
ostrowski vals = nub $ tail $ ostrowski' 0 1 vals
    where
        ostrowski' a b (v:vs) = a : ostrowski' b (b*v + a) vs

rep :: Integral a => [a] -> a -> [a]
rep reps n = rep' n reps $ reverse $ takeWhile (<= n) vals
    where
        vals = ostrowski reps

        rep' _ _ [] = []
        rep' x (maxDigit:ms) (d:ds)
            | x >= d = let m = min maxDigit $ x `div` d
                           newX = x - d*m
                       in m : rep' newX ms ds
            | otherwise = 0 : rep' x ms ds

zeroEndingParity :: (Eq a, Num a) => [a] -> Bool
zeroEndingParity = (== 0) . (`mod` 2) . genericLength . takeWhile (== 0) . reverse

sturmian :: Integral a => [a] -> [a]
sturmian reps = map (go . rep reps) [1..]
    where
        go val = if zeroEndingParity val then 0 else 1

repSturmian (rep:zRep) oRep (0:xs) = rep : repSturmian zRep oRep xs
repSturmian zRep (rep:oRep) (1:xs) = rep : repSturmian zRep oRep xs

runAutomata states input = (input, last (fst (foldl run ([], head states) input)))
    where
        run (out, state) c = fromJust $ do
            t <- find (\t -> t^.letter == c) $ state^.transitions
            st <- find (\st -> st^.num == t^.destState) states
            pure (out ++ [st^.output], st)

automataOutput states = map (snd . runAutomata states)

class WalnutOutput a where
    walnutStr :: a -> String

instance WalnutOutput a => WalnutOutput [a] where
    walnutStr = intercalate "\n" . map walnutStr

instance WalnutOutput Transition where
    walnutStr trans = show (trans^.letter) ++ " -> " ++ show (trans^.destState)

instance WalnutOutput State where
    walnutStr st = show (st^.num) ++ " " ++ show (st^.output) ++ "\n" ++
                    walnutStr (st^.transitions)

walnutOutput numSys states = numSys ++ "\n" ++ walnutStr states

