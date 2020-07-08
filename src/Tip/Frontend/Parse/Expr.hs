module Tip.Frontend.Parse.Expr (parseExpr) where

import Data.Either.Combinators (mapLeft)
import Text.Parsec
import Tip.Frontend.AST.Expr

type Parser a = Parsec String () a

-- Parses a string to an expression node.
parseExpr :: String -> String -> Either String Expr
parseExpr fp = mapLeft show . parse expr fp

-- Parses a string literal
litStrExpr :: Parser Expr
litStrExpr = LitStr <$> (char '"' *> many (noneOf ['"']) <* char '"')

-- Parses an integer literal
litIntExpr :: Parser Expr
litIntExpr = LitInt <$> read <$> many1 digit

-- Parses a function application
applyExpr :: Parser Expr
applyExpr = do
    f <- expr
    spaces
    x <- expr
    return $ Apply f x

-- Parses an identifier
ident :: Parser VarName
ident = many (noneOf [' '])

-- Parses a variable identifier
varExpr :: Parser Expr
varExpr = Var <$> ident

-- Parses a lambda
lambdaExpr :: Parser Expr
lambdaExpr = do
    char '\\'
    b <- varExpr
    spaces
    string "->"
    spaces
    e <- expr
    return $ Lambda b e

-- Parses a let expression
letExpr :: Parser Expr
letExpr = do
    string "let"
    spaces
    v <- ident
    spaces
    char '='
    spaces
    x <- expr
    spaces
    string "in"
    spaces
    y <- expr
    return $ Let v x y

-- Parses an expression
expr :: Parser Expr
expr = litStrExpr <|> litIntExpr <|> applyExpr <|> varExpr <|> lambdaExpr <|> letExpr
