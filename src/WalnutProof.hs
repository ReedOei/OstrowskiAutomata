{-# LANGUAGE OverloadedStrings #-}

module WalnutProof where

import Data.List
import Data.String

import Automata
import Lib
import Sturmian

data Command = Eval String (Maybe String) Query
             | Def String (Maybe String) Query
    deriving (Show, Eq)

data Query = Forall [String] Query
           | Exists [String] Query
           | Var String
           | IntVal Int
           | Symbol Int
           | Empty
           | Op Query String Query
           | ListOp String [Query]
           | SymbolAt String Query
           | Predicate String [Query]
           | Not Query
           | Reverse Query
    deriving (Show, Eq)

instance IsString Query where
    fromString = Var

instance Num Query where
    a + b = Op a "+" b
    a - b = Op a "-" b
    a * b = Op a "*" b
    fromInteger = IntVal . fromIntegral

(.==) :: Query -> Query -> Query
a .== b = Op a "=" b

varStr (Var s) = s

instance WalnutOutput Command where
    walnutStr (Eval name Nothing query) = "eval " ++ name ++ " \"" ++ walnutStr query ++ "\":"
    walnutStr (Eval name (Just numSys) query) = "eval " ++ name ++ " \"?" ++ numSys ++ " " ++ walnutStr query ++ "\":"

    walnutStr (Def name Nothing query) = "def " ++ name ++ " \"" ++ walnutStr query ++ "\":"
    walnutStr (Def name (Just numSys) query) = "def " ++ name ++ " \"?" ++ numSys ++ " " ++ walnutStr query ++ "\":"

instance WalnutOutput Query where
    walnutStr (Forall names query) = "A" ++ intercalate "," names ++ " " ++ walnutStr query
    walnutStr (Exists names query) = "E" ++ intercalate "," names ++ " " ++ walnutStr query
    walnutStr (Var name) = name
    walnutStr (IntVal i) = show i
    walnutStr (Symbol i) = "@" ++ show i
    walnutStr Empty = ""
    walnutStr (Op a opStr b) = "(" ++ walnutStr a ++ " " ++ opStr ++ " " ++ walnutStr b ++ ")"

    walnutStr (ListOp opStr xs) = go $ filter (not . null) $ map walnutStr xs
        where
            go [] = ""
            go [x] = x
            go strs = "(" ++ intercalate (" " ++ opStr ++ " ") strs ++ ")"

    walnutStr (SymbolAt word n) = word ++ "[" ++ walnutStr n ++ "]"
    walnutStr (Predicate name params) = "$" ++ name ++ "(" ++ intercalate "," (map walnutStr params) ++ ")"
    walnutStr (Not q) = "~(" ++ walnutStr q ++ ")"
    walnutStr (Reverse q) = "`(" ++ walnutStr q ++ ")"

validRep :: Query -> String -> Query -> Query
validRep base recogName n = Predicate recogName [base, n]

generalLte :: Int -> Command
generalLte maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Def ("lte_" ++ show maxChar) Nothing $
        Op (predicate "lt" ["x","y"]) "|" (predicate "eq" ["x","y"])

addCorrectBase :: Int -> Command
addCorrectBase maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Eval ("base_proof_" ++ show maxChar) Nothing $
        Exists ["z"] $ Op (predicate "zero" ["z"]) "&" $ Forall ["a", "x","y"] $
            Op (Op (predicate "recog" ["a","x"]) "&" (predicate "recog" ["a","y"])) "=>" $
                Op (predicate "eq" ["x", "y"]) "<=>" (predicate "add" ["a","x","z","y"])

doubleDef :: Int -> Command
doubleDef maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Def ("double_" ++ show maxChar) Nothing $ ListOp "&" $ recogAll maxChar "a" ["x","y"] ++ [predicate "add" ["a","x","x","y"]]

subDef :: Int -> Command
subDef maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Def ("sub_" ++ show maxChar) Nothing $ ListOp "&" $ recogAll maxChar "a" ["x","y","z"] ++ [predicate "add" ["a","z","y","x"]]

generalLtDef :: Int -> Command
generalLtDef maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Def ("lt_" ++ show maxChar) Nothing $ Reverse $ predicate "lt_temp" ["x","y"]

successorDef :: Int -> Command
successorDef maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Def ("successor_" ++ show maxChar) Nothing $
        ListOp "&" [predicate "recog" ["a","x"], predicate "recog" ["a","y"], predicate "lt" ["x","y"],
                    Forall ["k"] $ Op (predicate "recog" ["a","k"]) "=>" $
                        Op (predicate "lte" ["k","x"]) "|" $ Not $ predicate "lt" ["k","y"]]

recogAll :: Int -> String -> [Query] -> [Query]
recogAll maxChar a =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in map (\v -> predicate "recog" ["a",v])

addCorrect :: Int -> Command
addCorrect maxChar =
    let predicate name = Predicate (name ++ "_" ++ show maxChar)
    in Eval ("successor_proof_" ++ show maxChar) Nothing $
        Forall ["a","x","y","z","u","v"] $
            Op (ListOp "&" $ recogAll maxChar "a" ["x","y","z","u","v"] ++
                             [predicate "successor" ["a","y","u"], predicate "successor" ["a","z","v"]])
            "=>" $
                    Op (predicate "add" ["a","x","y","z"]) "<=>" (predicate "add" ["a","x","u","v"])

-- | The query to run to generate the general bounded addition automata
-- | The resulting automata takes in a, x, y, z, and accepts when x +_a y = z.
-- | Note the values should all be in LSD form
addAutomatonPrf :: Int -> Command
addAutomatonPrf maxChar =
    let recogName = "recog_" ++ show maxChar
        allValid = map (validRep "a" recogName) ["x","y"]
        alg0 = Predicate ("add_alg0_" ++ show maxChar) ["x","y","w"]
        alg1 = Predicate ("add_alg1_" ++ show maxChar) ["a","w","r"]
        alg2 = Predicate ("add_alg2_" ++ show maxChar) ["a","r","s"]
        alg3 = Predicate ("add_alg3_" ++ show maxChar) ["a","s","z"]
    in Def ("add_" ++ show maxChar) Nothing $
        Exists ["w", "r", "s"] $ ListOp "&" $ allValid ++ [alg0, Reverse alg1, alg2, Reverse alg3]

takeOneOfEach _ [] = []
takeOneOfEach [] _ = []
takeOneOfEach xs (y:ys)
    | snd y `elem` xs = y : takeOneOfEach (xs \\ [snd y]) ys
    | otherwise = takeOneOfEach xs ys
rotations xs = map (`rotate` xs) [0..length xs - 1]

startsCorrectProofs numSys cAlpha x zRep oRep reps =
    [startsCorrectProof numSys cAlpha x firstZero 0 (head zRep),
     startsCorrectProof numSys cAlpha x firstOne 1 (head oRep)]
    where
        allChars = nub $ zRep ++ oRep
        (firstZero, _) = head $ filter (\(i, v) -> v == 0) $ zip [1..] $ sturmian reps
        (firstOne, _) = head $ filter (\(i, v) -> v == 1) $ zip [1..] $ sturmian reps

startsCorrectProof :: String -> String -> String -> Int -> Int -> Int -> Command
startsCorrectProof numSys cAlpha x i startChar startRep =
    Eval ("first_" ++ show startChar ++ "_is_correct_" ++ x) (Just numSys) $
        Op (SymbolAt cAlpha (IntVal i) .== Symbol startChar) "&" (SymbolAt x (IntVal i) .== Symbol startRep)

vars = map (Var . ("x" ++) . show) [1..]

validReplacements x targetVars rep =
    ListOp "|" $ zipWith rotateToStr (repeat targetVars) $ map (take (length targetVars)) $ rotations rep
    where
        rotateToStr vSeq valSeq = ListOp "&" (zipWith go vSeq valSeq)
        go v val = SymbolAt x v .== Symbol val

betweenValsCorrect cAlpha [] between = Empty
betweenValsCorrect cAlpha [_] between = Empty
betweenValsCorrect cAlpha (first:vs) between =
    let notEqual
            | not $ null $ init vs = map (Op "i" "!=") (init vs)
            | otherwise = []
    in Forall ["i"] $
        Op (ListOp "&" ([Op "i" ">" first, Op "i" "<" (last vs)] ++ notEqual))
           "=>"
           (SymbolAt cAlpha "i" .== Symbol between)

symbolDist (x:xs) = symbolDist' 2 2 xs
    where
        symbolDist' dist _ [] = dist
        symbolDist' dist i (y:ys)
            | x == y = symbolDist' i (i + 1) ys
            | otherwise = symbolDist' dist (i + 1) ys

subseqLen :: Eq a => [a] -> Int
subseqLen = maximum . map symbolDist . rotations

replaceProof numSys target between cAlpha x rep =
    let len = min (length rep) (subseqLen rep)
        targetVars = take len vars
        constraintTargets = ListOp "&" (zipWith (\a b -> Op a "<" b) targetVars (tail targetVars))
        betweenIsCorrect = betweenValsCorrect cAlpha targetVars between
        targetsAreExpected = ListOp "&" $ map (\v -> SymbolAt cAlpha v .== Symbol target) targetVars
    in Eval ("replace_" ++ show target ++ "_between_" ++ show between ++ "_in_" ++ x) (Just numSys) $
        Forall (map varStr targetVars) $
            Op (ListOp "&" [constraintTargets, targetsAreExpected, betweenIsCorrect])
               "=>"
               (validReplacements x targetVars rep)

criticalExponentPrf numSys x zRep oRep
    -- Conjecture only holds for k >= 5
    | length alphabet >= 5 = [ gen "fac_low_exponent" "<=", gen "fac_ex_exponent" "=", gen "fac_high_exponent" ">=" ]
    | otherwise = []
    where
        alphabet = sort $ nub $ zRep ++ oRep
        n = length alphabet - 2
        d = length alphabet - 3
        gen name op = Eval (name ++ "_" ++ x) (Just numSys) $
            Exists ["i", "p", "n"] $
                ListOp "&" [Op "p" ">=" (IntVal 1),
                            Op (IntVal d * "n") op (IntVal n * "p"),
                            Forall ["j"] $ Op (Op ("j" + "p") "<" "n")
                                               "=>"
                                               (SymbolAt x ("i" + "j") .== SymbolAt x ("i" + "j" + "p"))]

