import Test.Hspec

import Lib

testAutomata :: [Int] -> [Int] -> [Int] -> [Int] -> ([Int], [Int]) -> ([Int], [Int]) -> Int -> IO ()
testAutomata alphabet zRep oRep reps zPeriod oPeriod n = compareSeqs fromAutomata actual
    where
        states = genAutomata alphabet zPeriod oPeriod zRep oRep
        fromAutomata = take n $ automataOutput states $ map (reverse . rep reps) [1..]
        actual = take n $ repSturmian (cycle zRep) (cycle oRep) $ sturmian reps

compareSeqs [] [] = pure ()
compareSeqs (x:xs) (y:ys) = do
    putStr $ "\r" ++ show (x, y, x == y)
    x `shouldBe` y
    compareSeqs xs ys

main :: IO ()
main = hspec $ do
    describe "genAutomata" $ do
        it "replace with [2,3] and [1]" $ testAutomata [0,1,2] [2,3] [1] (repeat 2) ([], [1]) ([], [0]) 10000
        it "replace with [0,1] and [2]" $ testAutomata [0,1,2] [2,3] [1] (repeat 2) ([], [1]) ([], [0]) 10000
        it "replace with [0,1,0,2] and [2]" $ testAutomata [0,1,2] [2,3] [1] (repeat 2) ([], [1,1,3,3]) ([], [0]) 10000
        it "replace with [0,1,0,2] and [3,4]" $ testAutomata [0,1,2] [2,3] [1] (repeat 2) ([], [1,1,3,3]) ([], [0,1]) 10000

