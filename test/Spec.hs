import Data.List

import Test.Hspec

import Automata
import qualified CAlpha
import Lib
import Sturmian

testAutomata :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int]) -> ([Int], [Int]) -> Int -> IO ()
testAutomata alphabet zRep oRep reps zPeriod oPeriod n = compareSeqs 0 fromAutomata actual
    where
        states = genAutomata alphabet zPeriod oPeriod zRep oRep
        fromAutomata = take n $ automataOutput states $ lsdReps reps
        actual = take n $ repSturmianWord (cycle zRep) (cycle oRep) reps

testMakeAutomata :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> IO ()
testMakeAutomata alphabet zRep oRep reps n = compareSeqs 0 fromAutomata actual
    where
        states = makeAutomata alphabet zRep oRep $ map fromIntegral reps
        fromAutomata = take n $ automataOutput states $ lsdReps reps
        actual = take n $ repSturmianWord (cycle zRep) (cycle oRep) reps

testOutputAutomata :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> IO ()
testOutputAutomata alphabet zRep oRep reps n = mapM_ doCompare automata
    where
        automata = makeOutputAutomata alphabet zRep oRep $ map fromIntegral reps
        doCompare (outputVal, states) = do
            putStrLn $ "Checking " ++ show outputVal ++ " (" ++ show (length states) ++ " states)"
            compareSeqs 0 fromAutomata actual
            putStrLn ""
            where
                fromAutomata = take n $ automataOutput states $ lsdReps reps
                actual = map (\x -> if x == outputVal then 1 else 0) $ take n $ repSturmianWord (cycle zRep) (cycle oRep) reps

testGenCAlpha :: [Int] -> [Int] -> Int -> IO ()
testGenCAlpha alphabet reps n = compareSeqs 0 fromAutomata actual
    where
        iReps = map fromIntegral reps
        states = CAlpha.genAutomata alphabet
        fromAutomata = take n $ automataOutput states $ map (map fromIntegral . reverse . rep iReps) [1..]
        actual = take n $ map fromIntegral $ sturmian iReps

compareSeqs _ [] [] = pure ()
compareSeqs i (x:xs) (y:ys) = do
    putStr $ "\r" ++ show (i, x, y, x == y)
    x `shouldBe` y
    compareSeqs (i + 1) xs ys

seqCheckLength = 100000

main :: IO ()
main = hspec $ do
    describe "makeOutputAutomata" $ do
        it "generates separate output automata for each of the output symbols" $ do
            let automata = makeOutputAutomata [0,1,2,3,4] [0,1] [2,3,4,5,6,7,2,8,4,9,6,3,2,5,4,7,6,8,2,9,4,3,6,5,2,7,4,8,6,9] (0:1:4:2:cycle [3])

            nub (sort (map fst automata)) `shouldMatchList` nub (sort [0,1,2,3,4,5,6,7,2,8,4,9,6,3,2,5,4,7,6,8,2,9,4,3,6,5,2,7,4,8,6,9])

        it "accepts output values for x3"  $ testOutputAutomata [0,1,2] [0,1] [2] (0:repeat 2) seqCheckLength
        it "accepts output values for x4"  $ testOutputAutomata [0,1] [0,1] [2,3] (0:2:repeat 1) seqCheckLength
        it "accepts output values for x5"  $ testOutputAutomata [0,1,2] [0,1,0,2] [3,4] (0:repeat 2) seqCheckLength
        it "accepts output values for x6"  $ testOutputAutomata [0,1,2] [0] [1,2,3,4,1,5,3,2,1,4,3,5] (0:1:2:1:1:cycle [1,1,1,2]) seqCheckLength
        it "accepts output values for x7"  $ testOutputAutomata [0,1,2,3] [0,1] [2,3,4,5,2,6,4,3,2,5,4,6] (0:1:1:3:cycle [1,2,1]) seqCheckLength
        it "accepts output values for x8"  $ testOutputAutomata [0,1,2,3] [0,1] [2,3,4,5,2,6,7,3,2,5,4,6,2,3,7,5,2,6,4,3,2,5,7,6] (0:1:3:1:cycle [2]) seqCheckLength
        it "accepts output values for x9"  $ testOutputAutomata [0,1,2,3] [0,1] [2,3,4,5,6,7,2,8,4,3,6,5,2,7,4,8,6,3,2,5,4,7,6,8] (0:1:2:3:cycle [2]) seqCheckLength
        it "accepts output values for x10" $ testOutputAutomata [0,1,2,3,4] [0,1] [2,3,4,5,6,7,2,8,4,9,6,3,2,5,4,7,6,8,2,9,4,3,6,5,2,7,4,8,6,9] (0:1:4:2:cycle [3]) seqCheckLength

    describe "CAlpha.genAutomata" $ do
        it "generates the sturmian word for x3"  $ testGenCAlpha [0,1,2] (0:repeat 2) seqCheckLength
        it "generates the sturmian word for x4"  $ testGenCAlpha [0,1] (0:2:repeat 1) seqCheckLength
        it "generates the sturmian word for x5"  $ testGenCAlpha [0,1,2] (0:repeat 2) seqCheckLength
        it "generates the sturmian word for x6"  $ testGenCAlpha [0,1,2] (0:1:2:1:1:cycle [1,1,1,2]) seqCheckLength
        it "generates the sturmian word for x7"  $ testGenCAlpha [0,1,2,3] (0:1:1:3:cycle [1,2,1]) seqCheckLength
        it "generates the sturmian word for x8"  $ testGenCAlpha [0,1,2,3] (0:1:3:1:cycle [2]) seqCheckLength
        it "generates the sturmian word for x9"  $ testGenCAlpha [0,1,2,3] (0:1:2:3:cycle [2]) seqCheckLength
        it "generates the sturmian word for x10" $ testGenCAlpha [0,1,2,3,4] (0:1:4:2:cycle [3]) seqCheckLength

    describe "genAutomata" $ do
        it "replace with [2,3] and [1]" $ testAutomata [0,1,2] [2,3] [1] (0:repeat 2) ([], [1]) ([], [0]) seqCheckLength
        it "replace with [0,1] and [2]" $ testAutomata [0,1,2] [0,1] [2] (0:repeat 2) ([], [1]) ([], [0]) seqCheckLength
        it "replace with [0,1,0,2] and [4]" $ testAutomata [0,1,2] [0,1,0,2] [4] (0:repeat 2) ([], [1,1,3,3]) ([], [0]) seqCheckLength
        it "replace with [0,1,0,2] and [3,4]" $ testAutomata [0,1,2] [0,1,0,2] [3,4] (0:repeat 2) ([], [1,1,3,3]) ([], [0,1]) seqCheckLength
        it "replace with [0,0,2] and [3,4,5,8]" $ testAutomata [0,1,2] [0,0,2] [3,4,5,8] (0:repeat 2) ([], [1,1,0,1,2,2,0,2]) ([], [0,1,2,1]) seqCheckLength

    describe "computeZPeriod" $ do
        it "compute the length 1 period rt2 for 0s" $ computeZPeriod (0:repeat 2) 1 `shouldBe` ([], [0])
        it "compute the length 2 period rt2 for 0s" $ computeZPeriod (0:repeat 2) 2 `shouldBe` ([], [1])
        it "compute the length 3 period rt2 for 0s" $ computeZPeriod (0:repeat 2) 3 `shouldBe` ([], [1,1,0,1,2,2,0,2])
        it "compute the length 4 period rt2 for 0s" $ computeZPeriod (0:repeat 2) 4 `shouldBe` ([], [1,1,3,3])
        it "compute the length 5 period rt2 for 0s" $ computeZPeriod (0:repeat 2) 5 `shouldBe` ([], [1,1,3,2,2,1,4,4,2,3,3,4])
        it "compute the length 6 period rt2 for 0s" $ computeZPeriod (0:repeat 2) 6 `shouldBe` ([], [1,1,3,1,5,5,3,5])

        it "compute the length 1 period for 1/phi^2 for 0s" $ computeZPeriod (0:2:repeat 1) 1 `shouldBe` ([], [0])
        it "compute the length 2 period for 1/phi^2 for 0s" $ computeZPeriod (0:2:repeat 1) 2 `shouldBe` ([], [1,1,0])
        it "compute the length 3 period for 1/phi^2 for 0s" $ computeZPeriod (0:2:repeat 1) 3 `shouldBe` ([], [1,1,2,0,2,2,1,0])
        it "compute the length 4 period for 1/phi^2 for 0s" $ computeZPeriod (0:2:repeat 1) 4 `shouldBe` ([], [1,1,2,3,1,0])
        it "compute the length 5 period for 1/phi^2 for 0s" $ computeZPeriod (0:2:repeat 1) 5 `shouldBe` ([], [1,1,2,3,0,3,3,1,4,0,4,4,3,2,0,2,2,4,1,0])
        it "compute the length 6 period for 1/phi^2 for 0s" $ computeZPeriod (0:2:repeat 1) 6 `shouldBe` ([], [1,1,2,3,5,2,1,3,4,1,5,0,5,5,4,3,1,4,5,3,2,5,1,0])

        it "compute the length 1 period for (78 - 2*sqrt(6))/101 for 0s" $ computeZPeriod (0:1:2:1:1:cycle [1,1,1,2]) 1 `shouldBe` ([], [0])
        it "compute the length 2 period for (78 - 2*sqrt(6))/101 for 0s" $ computeZPeriod (0:1:2:1:1:cycle [1,1,1,2]) 2 `shouldBe` ([1,0,1],[1,0,1,1,0,1,1,0])
        it "compute the length 3 period for (78 - 2*sqrt(6))/101 for 0s" $ computeZPeriod (0:1:2:1:1:cycle [1,1,1,2]) 3 `shouldBe` ([1,0,1],[1,2,0,2,2,0,2,2,1,1,2,0,2,1,0,1,1,0,1,1,2,2,1,0])
        it "compute the length 4 period for (78 - 2*sqrt(6))/101 for 0s" $ computeZPeriod (0:1:2:1:1:cycle [1,1,1,2]) 4 `shouldBe` ([1,0,1],[1,2,3,1,0,1,1,2,3,0,3,3,2,3,1,0])
        it "compute the length 5 period for (78 - 2*sqrt(6))/101 for 0s" $ computeZPeriod (0:1:2:1:1:cycle [1,1,1,2]) 5 `shouldBe` ([1,0,1],[1,2,3,0,3,1,4,0,4,3,2,0,2,4,1,0])
        it "compute the length 6 period for (78 - 2*sqrt(6))/101 for 0s" $ computeZPeriod (0:1:2:1:1:cycle [1,1,1,2]) 6 `shouldBe` ([1,0,1],[1,2,3,5,2,3,5,2,1,4,5,3,2,1,3,4,1,0,1,1,2,5,1,0])

    describe "computeOPeriod" $ do
        it "compute the length 1 period rt2 for 1s" $ computeOPeriod (0:repeat 2) 1 `shouldBe` ([], [0])
        it "compute the length 2 period rt2 for 1s" $ computeOPeriod (0:repeat 2) 2 `shouldBe` ([], [0,1])
        it "compute the length 3 period rt2 for 1s" $ computeOPeriod (0:repeat 2) 3 `shouldBe` ([], [0,1,2,2,0,2,1,1])
        it "compute the length 4 period rt2 for 1s" $ computeOPeriod (0:repeat 2) 4 `shouldBe` ([], [0,1,2,1])
        it "compute the length 5 period rt2 for 1s" $ computeOPeriod (0:repeat 2) 5 `shouldBe` ([], [0,1,2,0,2,4,0,4,3,0,3,1])
        it "compute the length 6 period rt2 for 1s" $ computeOPeriod (0:repeat 2) 6 `shouldBe` ([], [0,1,2,5,0,5,4,1])

        it "compute the length 1 period for 1/phi^2 for 1s" $ computeOPeriod (0:2:repeat 1) 1 `shouldBe` ([], [0])
        it "compute the length 2 period for 1/phi^2 for 1s" $ computeOPeriod (0:2:repeat 1) 2 `shouldBe` ([], [0,1,1])
        it "compute the length 3 period for 1/phi^2 for 1s" $ computeOPeriod (0:2:repeat 1) 3 `shouldBe` ([], [0,1,1,2,0,2,2,1])
        it "compute the length 4 period for 1/phi^2 for 1s" $ computeOPeriod (0:2:repeat 1) 4 `shouldBe` ([], [0,1,1,2,3,1])
        it "compute the length 5 period for 1/phi^2 for 1s" $ computeOPeriod (0:2:repeat 1) 5 `shouldBe` ([], [0,1,1,2,3,0,3,3,1,4,0,4,4,3,2,0,2,2,4,1])
        it "compute the length 6 period for 1/phi^2 for 1s" $ computeOPeriod (0:2:repeat 1) 6 `shouldBe` ([], [0,1,1,2,3,5,2,1,3,4,1,5,0,5,5,4,3,1,4,5,3,2,5,1])

        it "compute the length 1 period for (78 - 2*sqrt(6))/101 for 1s" $ computeOPeriod (0:1:2:1:1:cycle [1,1,1,2]) 1 `shouldBe` ([], [0])
        it "compute the length 2 period for (78 - 2*sqrt(6))/101 for 1s" $ computeOPeriod (0:1:2:1:1:cycle [1,1,1,2]) 2 `shouldBe` ([0,1,0],[1,1,0,1])
        it "compute the length 3 period for (78 - 2*sqrt(6))/101 for 1s" $ computeOPeriod (0:1:2:1:1:cycle [1,1,1,2]) 3 `shouldBe` ([0],[1,2,0,2,2,1,0,1])
        it "compute the length 4 period for (78 - 2*sqrt(6))/101 for 1s" $ computeOPeriod (0:1:2:1:1:cycle [1,1,1,2]) 4 `shouldBe` ([0,1,2],[3,1,0,1,1,3,0,3])
        it "compute the length 5 period for (78 - 2*sqrt(6))/101 for 1s" $ computeOPeriod (0:1:2:1:1:cycle [1,1,1,2]) 5 `shouldBe` ([0,1,2],[3,0,3,3,1,0,1,1,2,0,2,2,4,0,4,4])
        it "compute the length 6 period for (78 - 2*sqrt(6))/101 for 1s" $ computeOPeriod (0:1:2:1:1:cycle [1,1,1,2]) 6 `shouldBe` ([0,1,2],[3,5,2,1,3,1,4,5])

    describe "makeAutomata" $ do
        it "can produce x3" $ testMakeAutomata [0,1,2] [0,1] [2] (0:repeat 2) seqCheckLength
        it "can produce x4" $ testMakeAutomata [0,1] [0,1] [2,3] (0:2:repeat 1) seqCheckLength
        it "can produce x5" $ testMakeAutomata [0,1,2] [0,1,0,2] [3,4] (0:repeat 2) seqCheckLength
        it "can produce x6" $ testMakeAutomata [0,1,2] [0] [1,2,3,4,1,5,3,2,1,4,3,5] (0:1:2:1:1:cycle [1,1,1,2]) seqCheckLength
        it "can produce x7" $ testMakeAutomata [0,1,2,3] [0,1] [2,3,4,5,2,6,4,3,2,5,4,6] (0:1:1:3:cycle [1,2,1]) seqCheckLength
        it "can produce x8" $ testMakeAutomata [0,1,2,3] [0,1] [2,3,4,5,2,6,7,3,2,5,4,6,2,3,7,5,2,6,4,3,2,5,7,6] (0:1:3:1:cycle [2]) seqCheckLength
        it "can produce x9" $ testMakeAutomata [0,1,2,3] [0,1] [2,3,4,5,6,7,2,8,4,3,6,5,2,7,4,8,6,3,2,5,4,7,6,8] (0:1:2:3:cycle [2]) seqCheckLength
        it "can produce x10" $ testMakeAutomata [0,1,2,3,4] [0,1] [2,3,4,5,6,7,2,8,4,9,6,3,2,5,4,7,6,8,2,9,4,3,6,5,2,7,4,8,6,9] (0:1:4:2:cycle [3]) seqCheckLength

