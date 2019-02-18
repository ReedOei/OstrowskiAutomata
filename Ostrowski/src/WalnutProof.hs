module WalnutProof where

import Data.List

import Automata
import Lib

data Eval = Eval String String String
    deriving (Show, Eq)

instance WalnutOutput Eval where
    walnutStr (Eval name numSys query) = "eval " ++ name ++ " \"?" ++ numSys ++ " " ++ query ++ "\":"

startsCorrectProof numSys cAlpha x i startChar =
    Eval "start_correct" numSys $
        cAlpha ++ "[" ++ show i ++ "] = @" ++ startChar ++ " & " ++
        x ++ "[" ++ show i ++ "] = @" ++ startChar

vars = map (("x" ++) . show) [1..]

rotations xs = map (`rotate` xs) [0..length xs - 1]

validReplacements x targetVars rep =
    intercalate " | " $ zipWith rotateToStr (repeat targetVars) $ rotations rep
    where
        rotateToStr vSeq valSeq = "(" ++ intercalate " & " (zipWith go vSeq valSeq) ++ ")"
        go v val = "(" ++ x ++ "[" ++ v ++ "] = @" ++ show val ++ ")"

betweenValsCorrect cAlpha [] between = ""
betweenValsCorrect cAlpha [_] between = ""
betweenValsCorrect cAlpha (first:vs) between =
    let notEqual
            | not $ null $ init vs = " & (" ++ intercalate " & " (map (\v -> "(i != " ++ v ++ ")") (init vs)) ++ ")"
            | otherwise = ""
    in " & (Ai ((i > " ++ first ++ ") & (i < " ++ last vs ++ ")" ++ notEqual ++
       ") => (" ++ cAlpha ++ "[i] = @" ++ show between ++ "))"

0102
1020
0201
2010

symbolDist (x:xs) = symbolDist' 2 2 xs
    where
        symbolDist dist _ [] = dist
        symbolDist dist i (y:ys)
            | x == y = symbolDist i (i + 1) ys
            | otherwise = symbolDist dist (i + 1) ys

subseqLen = maximum (map symbolDist) . rotations

replaceProof numSys target between cAlpha x rep =
    let targetVars = take (length rep) vars
        forallTargetVars = "A" ++ intercalate "," targetVars
        constraintTargets
            | length rep > 1 = "(" ++ intercalate " & " (zipWith (\a b -> "(" ++ a ++ " < " ++ b ++ ")") targetVars (tail targetVars)) ++ ") & "
            | otherwise = ""
        betweenIsCorrect = betweenValsCorrect cAlpha targetVars between
        targetsAreExpected = intercalate " & " $ map (\v -> "(" ++ cAlpha ++ "[" ++ v ++ "] = @" ++ show target ++ ")") targetVars
    in Eval ("replace_" ++ show target ++ "_between_" ++ show between) numSys $
        forallTargetVars ++ " (" ++ constraintTargets ++
                                    targetsAreExpected ++
                                    betweenIsCorrect ++
                            ") => (" ++ validReplacements x targetVars rep ++ ")"

