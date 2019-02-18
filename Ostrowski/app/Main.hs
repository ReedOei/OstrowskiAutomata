module Main where

import Automata
import qualified CAlpha
import Lib
import WalnutProof

import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["c_alpha", wordName, numSys, alphabetStr] -> do
            let alphabet = read alphabetStr

            let states = CAlpha.genAutomata alphabet

            writeFile (wordName ++ ".txt") $ walnutOutput numSys states

        ["proof", wordName, numSys, cAlpha, zRepStr, oRepStr] -> do
            let zRep = read zRepStr :: [Integer]
            let oRep = read oRepStr :: [Integer]

            let repZeroPrf = replaceProof numSys 0 1 cAlpha wordName zRep
            let repOnePrf = replaceProof numSys 1 0 cAlpha wordName oRep

            writeFile (wordName ++ "_prf.txt") $ walnutStr repZeroPrf ++ "\n\n" ++ walnutStr repOnePrf
        ["generate", wordName, numSys, alphabetStr, zRepStr, oRepStr, nonRepStr, repsStr] -> do
            let alphabet = read alphabetStr
            let zRep = read zRepStr
            let oRep = read oRepStr
            let reps = read nonRepStr ++ cycle (read repsStr)

            let automata = makeAutomata alphabet zRep oRep reps

            writeFile (wordName ++ ".txt") $ walnutOutput numSys automata

