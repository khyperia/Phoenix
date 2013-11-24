module Parser (Parser.parse, addPrelude) where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative as App hiding (many, (<|>))
import Data.Maybe
import Data.Char
import Data.List
import Ast
import PhoenixPrelude

tokenSymbol :: String -> Parser String
tokenSymbol s = try (string s) <* spaces

tokenWord :: String -> Parser String
tokenWord s = try (string s <* notFollowedBy alphaNum) <* spaces

underscoreUpperIdentifier :: Parser String
underscoreUpperIdentifier = notFollowedBy keywordsParser *> ((\x y z -> x : y : z) <$> char '_' <*> upper <*> many alphaNum) <* spaces <?> "underscore identifier"

upperIdentifier :: Parser String
upperIdentifier = notFollowedBy keywordsParser *> ((:) <$> upper <*> many alphaNum) <* spaces <?> "uppercase identifier"

lowerIdentifier :: Parser String
lowerIdentifier = notFollowedBy keywordsParser *> ((:) <$> lower <*> many alphaNum) <* spaces <?> "lowercase identifier"

anyIdentifier :: Parser String
anyIdentifier = underscoreUpperIdentifier <|> upperIdentifier <|> lowerIdentifier <?> "identifier"

-- http://stackoverflow.com/questions/10726085
pinteger :: Parser Integer
pinteger = (foldl' (\a i -> a * 10 + fromIntegral (digitToInt i)) 0 <$> many1 digit) <* spaces <?> "integer"

keywordsParser :: Parser String
keywordsParser = foldl1 (<|>) (map string keywords)

keywords :: [String]
keywords = [ "in" ]

parentheses :: Parser a -> Parser a
parentheses = between (tokenSymbol "(") (tokenSymbol ")")

commaList :: Parser a -> Parser [a]
commaList = flip sepBy (tokenSymbol ",")

parenCommas :: Parser a -> Parser [a]
parenCommas = parentheses . commaList

parseType :: Parser Type
parseType = TypeAbstraction <$> (tokenWord "forall" *> pinteger) <*> (tokenSymbol "." *> parseType)
        <|> TypeIdentifier <$> pinteger
        <|> flip TypeConstructor [] <$> tokenWord "Int"
        <|> TypeConstructor <$> upperIdentifier <*> many parseType
        <|> parentheses parseType

parseLetBinding :: Parser (Expression -> Expression)
parseLetBinding = DataDef <$> (tokenWord "data" *> upperIdentifier) <*> (many pinteger  <* tokenSymbol "=") <*> sepBy1 ((,) <$> upperIdentifier <*> many parseType) (tokenSymbol "|")
              <|> (\n a v e -> Let n (foldr Abstraction v a) e) <$> lowerIdentifier <*> many lowerIdentifier <*> (tokenSymbol "=" *> parser)

parseBlock :: Parser Expression
parseBlock = flip ($) <$> parser <*> parseStatements

parseStatements :: Parser (Expression -> Expression)
parseStatements = (flip (foldr ($)) . catMaybes) <$> many (tokenSymbol ";" *> App.optional parseLetBinding)

parser :: Parser Expression
parser = foldl' Application <$> primary <*> many primary
  where primary = (tokenWord "let" *> parseLetBinding) <*> (tokenWord "in" *> parser)
              <|> Identifier <$> anyIdentifier
              <|> Constant <$> pinteger
              <|> flip (foldr Abstraction) <$> (tokenSymbol "\\" *> many1 lowerIdentifier) <*> (tokenSymbol "->" *> parser)
              <|> Identifier <$> tokenWord "#fix"
              <|> Primop <$> (char '#' *> primParse)
              <|> createTuple <$> parenCommas parseBlock
              <|> foldr (\l r -> Application (Application (Identifier "Cons") l) r) (Identifier "Empty") <$> between (tokenSymbol "[") (tokenSymbol "]") (commaList parser)
        primParse = const PrimAdd <$> tokenWord "add"
        createTuple [] = Identifier "Unit"
        createTuple [x] = x
        createTuple xs = foldl Application (Identifier $ "Tuple" ++ show (length xs)) xs

addPrelude :: Expression -> Expression
addPrelude = preludeParsed
    where preludeParsed = either (error . show) id $ runParser (parseStatements <* eof) () "prelude" prelude

parse :: String -> Expression
parse = either (error . show) id . runParser (parseBlock <* eof) () "fzoo"
