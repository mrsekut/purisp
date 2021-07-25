module Test.MalSpecRunner where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (try)
import Data.Array as Array
import Data.Either (Either(..))
import Data.List (List, many, (:))
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (skipMany, skipMany1)
import Text.Parsing.Parser.String (eof, noneOf, oneOf, string)
import Text.Parsing.Parser.Token (space)


-- Types


data MalSpec = Input String
             | Output String

instance Show MalSpec where
  show (Input ms) = "inputmal: " <> show ms
  show (Output ms) = "outputmal: " <> show ms



-- Main

main :: Effect Unit
main = do
  strs <- readTextFile UTF8 "./test/MalSpecs/test1.mal"
  a <- try $ readStr strs
  log $ show a



--

readStr :: String -> Effect (List MalSpec)
readStr str = case runParser str parseMalSpec of
  Left _    -> throw "EOF"
  Right val -> pure val



-- Lines

parseMalSpec :: Parser String (List MalSpec)
parseMalSpec = lines parseLine


lines :: ∀ a. Parser String a -> Parser String (List a)
lines line = do
  l <- line
  ls <- many $ eol *> line
  eof
  pure $ l:ls



-- Line

parseLine :: Parser String MalSpec
parseLine = ignored *> (parseOutput <|> parseInput)


parseInput :: Parser String MalSpec
parseInput = Input <$> parseStringTillEOL


parseOutput :: Parser String MalSpec
parseOutput = do
  skipMany1 $ string ";=>"
  Output <$> parseStringTillEOL


ignored :: Parser String Unit
ignored = skipMany $ skipMany1 space <|> comment



-- Utils

comment :: Parser String Unit
comment = commentSymbol *> (skipMany $ oneOf ['\r', '\n'])


commentSymbol :: Parser String String
commentSymbol = string ";;" <|> string ";;;"


parseStringTillEOL :: Parser String String
parseStringTillEOL = charListToString <$> (many $ noneOf ['\r', '\n'])


charListToString :: List Char -> String
charListToString = fromCharArray <<< Array.fromFoldable


eol :: Parser String Char
eol = oneOf ['\n', '\r']