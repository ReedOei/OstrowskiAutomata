{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TheoremFinder where

import Control.Lens
import Control.Monad.IO.Class

import Data.List
import qualified Data.Text as T

import Turtle hiding (sort, nub)

import Automata
import AutomataParser
import CAlpha

data NumSys = NumSys
    { _name :: String
    , _nonRep :: [Int]
    , _rep :: [Int] }
    deriving (Show, Eq)
makeLenses ''NumSys

walnutWordsPath :: String -> String
walnutWordsPath walnutPath = walnutPath ++ "/Word Automata Library"

walnutWordPath :: String -> String -> String
walnutWordPath walnutPath wordName = walnutWordsPath walnutPath ++ "/" ++ wordName ++ ".txt"

makeNumSys :: String -> NumSys -> Shell ()
makeNumSys walnutPath numSys = do
    let args = [numSys^.name, show (numSys^.nonRep), show (numSys^.rep)]
    let command = T.pack $ "bash gen-addition.sh " ++ unwords args

    inshell command empty

    let states = CAlpha.genAutomata $ map (:[]) $ sort $ nub $ (numSys^.nonRep) ++ (numSys^.rep)
    let outputPath = walnutWordPath walnutPath (numSys^.name)
    liftIO $ writeUtf16File outputPath $ walnutOutput ("lsd_" ++ numSys^.name) states

-- tryTheorems :: MonadIO m => String -> NumSys -> [String] -> m [Bool]
-- tryTheorems walnutPath numSys = mapM (tryTheorem numSys)

-- tryTheorem :: MonadIO m => String -> NumSys -> String -> m Bool
-- tryTheorem walnutPath numSys query = do
--     makeNumSys walnutPath numSys


