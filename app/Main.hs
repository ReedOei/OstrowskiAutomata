module Main where

import Data.List

import Automata
import AutomataParser
import qualified CAlpha
import Lib
import TheoremFinder
import WalnutProof

import System.Directory
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["minimize", fname] -> do
            (numSys, states) <- parseAutomata <$> readUtf16File fname

            let alphabet = alphabetOf states
            putStrLn $ fname ++ ": Automaton originally had " ++ show (length states) ++ " states."
            let minimized = minimizeAutomata alphabet $ states
            putStrLn $ fname ++ ": Minimized automaton now has " ++ show (length minimized) ++ " states."

            copyFile fname $ fname ++ ".bak"
            writeUtf16File fname $ walnutOutput numSys minimized
        ["c_alpha", wordName, numSys, alphabetStr] -> do
            let alphabet = read alphabetStr

            let states = CAlpha.genAutomata alphabet

            writeFile (wordName ++ ".txt") $ walnutOutput numSys states

        ["proof", wordName, numSys, cAlpha, zRepStr, oRepStr, nonRepStr, repsStr] -> do
            let zRep = read zRepStr
            let oRep = read oRepStr
            let reps = read nonRepStr ++ cycle (read repsStr)

            let startPrfs = startsCorrectProofs numSys cAlpha wordName zRep oRep reps
            let repZeroPrf = replaceProof numSys 0 1 cAlpha wordName zRep
            let repOnePrf = replaceProof numSys 1 0 cAlpha wordName oRep
            let criticalExpPrfs = criticalExponentPrf numSys wordName zRep oRep

            let prfs = startPrfs ++ criticalExpPrfs ++ [repZeroPrf, repOnePrf]

            writeFile (wordName ++ "_prf.txt") $ intercalate "\n\n" $ map walnutStr prfs
        ["generate", wordName, numSys, alphabetStr, zRepStr, oRepStr, nonRepStr, repsStr] -> do
            let alphabet = read alphabetStr
            let zRep = read zRepStr
            let oRep = read oRepStr
            let reps = read nonRepStr ++ cycle (read repsStr)

            let automata = prune $ makeAutomata alphabet zRep oRep reps

            writeFile (wordName ++ ".txt") $ walnutOutput numSys automata
        ["generate_output", wordName, numSys, alphabetStr, zRepStr, oRepStr, nonRepStr, repsStr] -> do
            let alphabet = read alphabetStr
            let zRep = read zRepStr
            let oRep = read oRepStr
            let reps = read nonRepStr ++ cycle (read repsStr)

            let automata = makeOutputAutomata alphabet zRep oRep reps

            mapM_ (\(outputVal, states) -> writeFile (wordName ++ "_" ++ show outputVal ++ ".txt") $ walnutOutput numSys states) automata

