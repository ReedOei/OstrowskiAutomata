#!/usr/bin/env stack
{- stack
   script
   --resolver lts-13.7
   --package containers
   --package text
   --package bytestring
   --package lens
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Lens

import qualified Data.ByteString as B
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import qualified Data.Text as Text
import Data.Text.Encoding

import System.Environment

import Debug.Trace

concats :: [[a]] -> [[a]]
concats [] = [[]]
concats (xs:xss) = [ y:ys | y <- xs, ys <- concats xss ]

ranges :: Enum a => a -> [a] -> [[a]]
ranges start = map (enumFromTo start)

pairAllWith :: (a -> b -> c) -> [a] -> [b] -> [c]
pairAllWith f as bs = [f a b | a <- as, b <- bs]

pairAll :: [a] -> [b] -> [(a, b)]
pairAll = pairAllWith (,)

data Transition = Transition
    { _letter :: Integer
    , _destState :: Integer }
    deriving (Show, Eq)
makeLenses ''Transition

data State = State
    { _num :: Integer
    , _output :: Integer
    , _transitions :: [Transition]
    , _isEven :: Bool
    , _evenCount :: Integer
    , _oddCount :: Integer
    , _ending :: [Integer] }
    deriving (Show, Eq)
makeLenses ''State

ostrowski vals = nub $ tail $ ostrowski' 0 1 vals
    where
        ostrowski' a b (v:vs) = a : ostrowski' b (b*v + a) vs

rep reps n = rep' n reps $ reverse $ takeWhile (<= n) vals
    where
        vals = ostrowski reps

        rep' _ _ [] = []
        rep' x (maxDigit:ms) (d:ds)
            | x >= d = let m = min maxDigit $ x `div` d
                           newX = x - d*m
                       in m : rep' newX ms ds
            | otherwise = 0 : rep' x ms ds

zeroEndingParity = (== 0) . (`mod` 2) . genericLength . takeWhile (== 0) . reverse

-- takeWhileUnique = takeWhileUnique' []
--     where
--         takeWhileUnique' seen (x:xs)
--             | x `elem` seen = seen
--             | otherwise = takeWhileUnique' (x:seen) xs

sturmian reps = map (go . rep reps) [1..]
    where
        go val = if zeroEndingParity val then 0 else 1

periodic = concat . repeat

repSturmian (rep:zRep) oRep (0:xs) = rep : repSturmian zRep oRep xs
repSturmian zRep (rep:oRep) (1:xs) = rep : repSturmian zRep oRep xs

examples reps zRep oRep = zip (map (rep reps) [1..]) $ repSturmian zRep oRep $ sturmian reps

exampleStrs reps zRep oRep = map toStr $ examples reps (periodic zRep) (periodic oRep)
    where
        toStr (input, out) = show input ++ " -> " ++ show out

main = do
    args <- getArgs

    case args of
        [fname, nStr, nStart, nRep, zRepStr, oRepStr] -> do
            let n = read nStr
            let zRep = read zRepStr :: [Integer]
            let oRep = read oRepStr :: [Integer]

            let reps = read nStart ++ periodic (read nRep)
            let strs = intercalate "\n" $ take n $ exampleStrs reps zRep oRep

            writeFile fname strs
        _ -> do
            putStrLn "Usage: ./Lib.hs OUTPUT_FILE EXAMPLE_NUM FRAC_START FRAC_REPEAT ZERO_REP ONE_REP"
            putStrLn "OUTPUT_FILE - The file to write the examples to"
            putStrLn "EXAMPLE_NUM - How many examples to write"
            putStrLn "FRAC_START  - The beginning (non-repeating) part of the continued fraction"
            putStrLn "FRAC_REPEAT - The repeating part of the continued fraction"
            putStrLn "ZERO_REP    - The (constant gap) periodic sequence to replace the 0s with"
            putStrLn "ONE_REP     - The (constant gap) periodic sequence to replace the 1s with"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- OLD STUFF. DOESN'T WORK ATM
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

padL len e xs = replicate (fromIntegral (len - genericLength xs)) e ++ xs

endings len alphabet = concats $ replicate (fromIntegral len) alphabet

attachAll = pairAll [True,False] . pairAll [0] . pairAll [0]
attachTF = pairAll [True, False]

pad len = map (\(a, (b, c)) -> (a, (b, padL len 0 c)))

stateLabels n m alphabet reps = labels
    where
        len = stateLabelLen n m reps
        eq (atf, (_, (_, aending)))
           (btf, (_, (_, bending))) = atf == btf && aending == bending
        labels = nubBy eq $ attachTF (pad len (stateLabelBase n m reps)) ++ attachAll (endings len alphabet)

stateLabelLen n m reps = maximum $ map (genericLength . snd . snd) $ stateLabelBase n m reps

stateLabelBase n m reps = map (\(i, (j, nRep)) -> (i `mod` n, (j `mod` m, nRep))) $ gen' 0 0 $ map (rep reps) [1..]
    where
        -- Never empty, so don't need to account for that case
        gen' i j (nRep:ns)
            | i > n && j > m = [(i, (j, nRep))]
            | zeroEndingParity nRep = (i, (j, nRep)) : gen' (i + 1) j ns
            | otherwise = (i, (j, nRep)) : gen' i (j + 1) ns

genStates alphabet reps zeroRep oneRep = zipWith (genState zeroRep oneRep) [1..] $ filter valid labels
    where
        labels = stateLabels (genericLength zeroRep) (genericLength oneRep) alphabet reps

valid (isEven, (evenCount, (oddCount, ending))) =
    all (==0) ending ||
    isEven == zeroEndingParity ending

genState zeroRep oneRep num (isEven, (evenCount, (oddCount, ending))) =
    State
    { _num = num
    , _output = if isEven then zeroRep !! fromIntegral evenCount else oneRep !! fromIntegral oddCount
    , _transitions = []
    , _isEven = isEven
    , _evenCount = evenCount
    , _oddCount = oddCount
    , _ending = ending }

stateFor even end state =
    state^.ending == end &&
    state^.isEven == even

modAdd n a b = (a + b) `mod` n

transitionTo states letter newIsEven newEnding =
    case find (stateFor newIsEven newEnding) states of
        Just st -> [Transition letter $ st^.num]
        Nothing -> []

genTransition n m states state letter =
    transitionTo states letter newIsEven newEnding
    where
        newIsEven = (letter /= 0) || not (state^.isEven)
        newEnding = tail $ state^.ending ++ [letter]

buildStates alphabet reps zeroRep oneRep =
    startState : map (\state -> set transitions (transitionsFor state) state) states
    where
        transitionsFor state = concatMap (genTransition n m states state) alphabet
        n = genericLength zeroRep
        m = genericLength oneRep
        len = stateLabelLen n m reps
        states = genStates alphabet reps zeroRep oneRep
        finalStates = map (\state -> set transitions (transitionsFor state) state) states
        startState = State 0 0 ts True 0 0 $ replicate (fromIntegral len) 0
            where
                ts = concatMap (\l -> transitionTo states l (l /= 0) $ replicate (fromIntegral (len - 1)) 0 ++ [l]) alphabet

runAutomata states input = (input, last (fst (foldl run ([], head states) input)))
    where
        run (out, state) c = fromJust $ do
            t <- find (\t -> t^.letter == c) $ state^.transitions
            st <- find (\st -> st^.num == t^.destState) states
            pure (out ++ [st^.output], st)

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
toUtf16 = encodeUtf16BE . Text.pack
writeUtf16 fname = B.writeFile fname . toUtf16

