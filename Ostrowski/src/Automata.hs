{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Automata where

import Control.Lens

import Data.List
import Data.Maybe

data Transition = Transition
    { _letter :: Int
    , _destState :: Int }
    deriving (Show, Eq)
makeLenses ''Transition

data State a = State
    { _num :: Int
    , _output :: Int
    , _transitions :: [Transition]
    , _info :: a }
    deriving (Show, Eq)
makeLenses ''State

runAutomata :: [State a] -> [Int] -> ([Int], Int)
runAutomata states input = (input, last (fst (foldl run ([], head states) input)))
    where
        run (out, state) c = fromJust $ do
            t <- find (\t -> t^.letter == c) $ state^.transitions
            st <- find (\st -> st^.num == t^.destState) states
            pure (out ++ [st^.output], st)

automataOutput :: [State a] -> [[Int]] -> [Int]
automataOutput states = map (snd . runAutomata states)

class WalnutOutput a where
    walnutStr :: a -> String

instance WalnutOutput a => WalnutOutput [a] where
    walnutStr = intercalate "\n" . map walnutStr

instance WalnutOutput Transition where
    walnutStr trans = show (trans^.letter) ++ " -> " ++ show (trans^.destState)

instance WalnutOutput (State a) where
    walnutStr st = show (st^.num) ++ " " ++ show (st^.output) ++ "\n" ++
                    walnutStr (st^.transitions)

walnutOutput numSys states = numSys ++ "\n" ++ walnutStr states

