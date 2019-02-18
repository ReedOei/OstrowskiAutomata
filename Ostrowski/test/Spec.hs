import Test.Hspec

import Lib

testAutomata :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int]) -> ([Int], [Int]) -> Int -> IO ()
testAutomata alphabet zRep oRep reps zPeriod oPeriod n = compareSeqs 0 fromAutomata actual
    where
        states = genAutomata alphabet zPeriod oPeriod zRep oRep
        fromAutomata = take n $ automataOutput states $ map (reverse . rep reps) [1..]
        actual = take n $ repSturmian (cycle zRep) (cycle oRep) $ sturmian reps

compareSeqs _ [] [] = pure ()
compareSeqs i (x:xs) (y:ys) = do
    putStr $ "\r" ++ show (i, x, y, x == y)
    x `shouldBe` y
    compareSeqs (i + 1) xs ys

main :: IO ()
main = hspec $ do
    describe "genAutomata" $ do
        it "replace with [2,3] and [1]" $ testAutomata [0,1,2] [2,3] [1] (repeat 2) ([], [1]) ([], [0]) 10000
        it "replace with [0,1] and [2]" $ testAutomata [0,1,2] [0,1] [2] (repeat 2) ([], [1]) ([], [0]) 10000
        it "replace with [0,1,0,2] and [4]" $ testAutomata [0,1,2] [0,1,0,2] [4] (repeat 2) ([], [1,1,3,3]) ([], [0]) 10000
        it "replace with [0,1,0,2] and [3,4]" $ testAutomata [0,1,2] [0,1,0,2] [3,4] (repeat 2) ([], [1,1,3,3]) ([], [0,1]) 10000
        it "replace with [0,0,2] and [3,4,5,8]" $ testAutomata [0,1,2] [0,0,2] [3,4,5,8] (repeat 2) ([], [1,1,0,1,2,2,0,2]) ([], [0,1,2,1]) 10000

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

