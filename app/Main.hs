module Main where

import Data.List

import Automata
import AutomataParser
import qualified CAlpha
import Lib
import NumerationSystem
import TheoremFinder
import WalnutProof

import System.Directory
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["general", "recog", maxCharStr] -> do
            let maxChar = read maxCharStr

            let fracAlphabet = "{" ++ intercalate "," (map show [1..maxChar]) ++ "}"
            let digitAlphabet = "{" ++ intercalate "," (map show [0..maxChar]) ++ "}"

            let states = generalRecogAutomata maxChar

            writeUtf16File ("general_" ++ maxCharStr ++ ".txt") $ walnutOutput (fracAlphabet ++ " " ++ digitAlphabet) states
        ["minimize", fname] -> do
            (numSys, states) <- parseAutomata <$> readUtf16File fname

            let alphabet = alphabetOf states
            putStrLn $ fname ++ ": Automaton originally had " ++ show (length states) ++ " states."
            let minimized = minimizeAutomata alphabet states
            putStrLn $ fname ++ ": Minimized automaton now has " ++ show (length minimized) ++ " states."

            copyFile fname $ fname ++ ".bak"
            writeUtf16File fname $ walnutOutput numSys minimized
        ["c_alpha", wordName, numSys, alphabetStr] -> do
            let alphabet = read alphabetStr

            let states = CAlpha.genAutomata alphabet

            writeFile (wordName ++ ".txt") $ walnutOutput numSys states
        ["c_alpha", wordName, numSys, nonRepStr, repStr] -> do
            -- A space list is a list like this: 0 4 1 3 1
            -- Each symbol is assumed to be a letter of the alphabet, and so is parsed like this: [[0],[4],[1],[3],[1]]
            let readSpaceList = map (:[]) . map read . words
            let alphabet = sort $ nub $ readSpaceList nonRepStr ++ readSpaceList repStr
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

