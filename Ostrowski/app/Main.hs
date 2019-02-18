module Main where

import Lib

import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["generate", wordName, numSys, alphabetStr, zRepStr, oRepStr, nonRepStr, repsStr] -> do
            let alphabet = read alphabetStr
            let zRep = read zRepStr
            let oRep = read oRepStr
            let reps = read nonRepStr ++ cycle (read repsStr)

            let automata = makeAutomata alphabet zRep oRep reps

            writeFile (wordName ++ ".txt") $ walnutOutput numSys automata

