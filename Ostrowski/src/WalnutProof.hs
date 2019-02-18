module WalnutProof where

import Data.List

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

betweenValsCorrect cAlpha targetVars between =
    "(" ++ intercalate " & " (zipWith go targetVars (tail targetVars)) ++ ")"
    where
        go v1 v2 = "((i > " ++ v1 ++ ") & (i < " ++ v2 ++ ")) => (" ++ cAlpha ++ "[i] = @" ++ show between ++ ")"

replaceProof numSys target between cAlpha x rep =
    let targetVars = take (length rep) vars
        forallTargetVars = "A" ++ intercalate "," targetVars
        constraintTargets
            | length rep > 1 = "(" ++ intercalate " & " (zipWith (\a b -> "(" ++ a ++ " < " ++ b ++ ")") targetVars (tail targetVars)) ++ ") & "
            | otherwise = ""
        betweenIsCorrect
            | length rep > 1 = " & (Ai (" ++ betweenValsCorrect cAlpha targetVars between ++ "))"
            | otherwise = ""
        targetsAreExpected = intercalate " & " $ map (\v -> "(" ++ cAlpha ++ "[" ++ v ++ "] = @" ++ show target ++ ")") targetVars
    in Eval ("replace_" ++ show target ++ "_between_" ++ show between) numSys $
        forallTargetVars ++ " (" ++ constraintTargets ++
                                    targetsAreExpected ++
                                    betweenIsCorrect ++
                            ") => (" ++ validReplacements x targetVars rep ++ ")"
