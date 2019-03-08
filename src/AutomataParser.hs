{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module AutomataParser where

import Control.Monad

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding

import Text.Parsec hiding (State)
import Text.Parsec.Number

import Automata hiding (transition, transitions)

type Parser a = forall s st m. Stream s m Char => ParsecT s st m a

writeUtf16File :: FilePath -> String -> IO ()
writeUtf16File path str = B.writeFile path $ encodeUtf16BE $ T.pack str

readUtf16File :: FilePath -> IO String
readUtf16File = B.readFile >=> pure . T.unpack . decodeUtf16BE

parseAutomata :: String -> (String, [State ()])
parseAutomata str =
    case parse automata "" str of
        Left err -> error $ show err
        Right res -> res

automata :: Parser (String, [State ()])
automata = do
    optional $ char '\65279' -- Skip BOM, we assume the file is already UTF16-BE anyway
    numSys <- many (noneOf "\n")
    newlines
    states <- sepEndBy state (optional newlines)
    pure (numSys, states)

state :: Parser (State ())
state = do
    num <- int
    wsSkip
    output <- int
    transitions <- transitions

    pure $ State num output transitions ()

transitions :: Parser (Map [Int] Int)
transitions = do
    t <- try (optionMaybe $ do
        newlines
        v <- transition
        pure v) <|> (optional newlines >> pure Nothing)

    case t of
        Nothing -> pure Map.empty
        Just newTrans -> uncurry Map.insert <$> pure newTrans <*> transitions

letters :: Parser [Int]
letters = do
    wsSkip
    n <- optionMaybe (try int)

    case n of
        Nothing -> pure []
        Just i  -> (:) <$> pure i <*> letters

transition :: Parser ([Int], Int)
transition = do
    letter <- letters
    symbol $ string "->"
    destState <- int
    pure (letter, destState)

symbol :: Stream s m Char => ParsecT s st m a -> ParsecT s st m a
symbol parser = do
    lineSep
    v <- parser
    lineSep
    pure v

newlines :: Parser ()
newlines = skipMany1 (wsSkip >> char '\n' >> wsSkip)

wsSkip :: Parser ()
wsSkip = skipMany $ oneOf " \r\t"

lineSep :: Parser ()
lineSep = do
    wsSkip
    optional newlines
    wsSkip

