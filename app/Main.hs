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

makeAlphabetStr :: [[Int]] -> String
makeAlphabetStr = unwords . map go
    where
        go alphabet = "{" ++ intercalate "," (map show alphabet) ++ "}"

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["run", fname, inputStr] -> do
            let input = read inputStr
            (numSys, states) <- parseAutomata <$> readUtf16File fname

            print $ head $ automataOutput states [input]

        ["general", "recog", maxCharStr] -> do
            let maxChar = read maxCharStr

            let fracAlphabet = makeAlphabetStr [[1..maxChar]]
            let digitAlphabet = makeAlphabetStr [[0..maxChar]]

            let states = generalRecogAutomata maxChar

            writeUtf16File ("general_" ++ maxCharStr ++ ".txt") $ walnutOutput (fracAlphabet ++ " " ++ digitAlphabet) states

            let calpha = CAlpha.genAutomata $ map (:[]) [0..maxChar]
            writeUtf16File ("C_general_" ++ maxCharStr ++ ".txt") $ walnutOutput digitAlphabet calpha

        ["general", "add", maxCharStr] -> do
            let maxChar = read maxCharStr

            let fracAlphabet = [1..maxChar]
            let sumAlphabet = [0..maxChar + maxChar]
            let digitAlphabet = [0..maxChar]
            let alg0Alphabet = makeAlphabetStr [digitAlphabet, digitAlphabet, sumAlphabet]
            let alg1Alphabet = makeAlphabetStr [fracAlphabet, sumAlphabet, digitAlphabet]
            let alg2Alphabet = makeAlphabetStr [fracAlphabet, digitAlphabet, digitAlphabet]
            let alg3Alphabet = makeAlphabetStr [fracAlphabet, digitAlphabet, digitAlphabet]

            let alg0 = alg0Automaton maxChar
            let alg1 = alg1Automaton maxChar
            let alg2 = alg2Automaton maxChar
            let alg3 = alg3Automaton maxChar

            writeUtf16File ("general_add_alg0_" ++ maxCharStr ++ ".txt") $ walnutOutput alg0Alphabet alg0
            -- writeUtf16File ("general_add_alg1_" ++ maxCharStr ++ ".txt") $ walnutOutput alg1Alphabet alg1
            writeUtf16File ("general_add_alg2_" ++ maxCharStr ++ ".txt") $ walnutOutput alg2Alphabet alg2
            writeUtf16File ("general_add_alg3_" ++ maxCharStr ++ ".txt") $ walnutOutput alg3Alphabet alg3

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

