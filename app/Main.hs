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

makeGeneralAutomata :: String -> Int -> [[Int]] -> (Int -> [State a]) -> IO [State a]
makeGeneralAutomata name maxChar alphabet gen = do
    let states = gen maxChar
    let alphabetStr = makeAlphabetStr alphabet

    let fname = "general_" ++ name ++ "_" ++ show maxChar ++ ".txt"
    putStrLn $ "Generated (" ++ show (length states) ++ " states): " ++ fname

    writeUtf16File fname $ walnutOutput alphabetStr states

    pure states

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["run", fname, inputStr] -> do
            let input = read inputStr
            (numSys, states) <- parseAutomata <$> readUtf16File fname

            print $ head $ automataOutput states [input]
        ["general", maxCharStr] -> do
            let maxChar = read maxCharStr

            let fracAlphabet = [1..maxChar]
            let sumAlphabet = [0..maxChar + maxChar + maxChar]
            let digitAlphabet = [0..maxChar]

            makeGeneralAutomata "recog" maxChar [fracAlphabet, digitAlphabet] generalRecogAutomata
            makeGeneralAutomata "lt_temp" maxChar [digitAlphabet, digitAlphabet] generalLt
            makeGeneralAutomata "eq" maxChar [digitAlphabet, digitAlphabet] generalEq
            makeGeneralAutomata "zero" maxChar [digitAlphabet] $ const generalZero
            makeGeneralAutomata "one" maxChar [fracAlphabet, digitAlphabet] generalOne
            makeGeneralAutomata "add_alg0" maxChar [digitAlphabet, digitAlphabet, sumAlphabet] alg0Automaton
            makeGeneralAutomata "add_alg1" maxChar [fracAlphabet, sumAlphabet, digitAlphabet] alg1Automaton
            makeGeneralAutomata "add_alg2" maxChar [fracAlphabet, digitAlphabet, digitAlphabet] alg2Automaton
            makeGeneralAutomata "add_alg3" maxChar [fracAlphabet, digitAlphabet, digitAlphabet] alg3Automaton

            let prfs = map ($ maxChar) [addAutomatonPrf, generalLtDef, generalLte, addCorrectBase, successorDef, addCorrect]

            writeFile ("general_" ++ maxCharStr ++ "_prfs.txt") $ intercalate "\n\n" $ map walnutStr prfs

            let calpha = CAlpha.genAutomata $ map (:[]) [0..maxChar]
            writeUtf16File ("C_general_" ++ maxCharStr ++ ".txt") $ walnutOutput (makeAlphabetStr [digitAlphabet]) calpha

        ["inputs", fname, targetOutputStr] -> do
            (numSys, states) <- parseAutomata <$> readUtf16File fname

            let targetOutput = read targetOutputStr
            mapM_ print $ findInputs targetOutput states

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

