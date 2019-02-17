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

