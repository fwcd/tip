module Tip.Frontend.Parse.Expr (parseExpr) where

import Data.Either.Combinators (mapLeft)
import Text.Parsec
import Tip.Frontend.AST.Expr
import Tip.Frontend.AST.VarName

type Parser a = Parsec String () a

-- Parses a string to an expression node.
parseExpr :: String -> String -> Either String (Expr ())
parseExpr fp = mapLeft show . parse expr fp

-- Parses a string literal
litStrExpr :: Parser (Expr ())
litStrExpr = LitStr <$> pure () <*> (char '"' *> many (noneOf ['"']) <* char '"')

-- Parses an integer literal
litIntExpr :: Parser (Expr ())
litIntExpr = LitInt <$> pure () <*> read <$> many1 digit

-- Parses a function application
applyExpr :: Parser (Expr ())
applyExpr = do
    -- TODO: Handle precedence and left-recursion so that
    --       not every application has to be parenthesized
    _ <- char '('
    spaces
    f <- expr
    spaces
    x <- expr
    spaces
    _ <- char ')'
    pure $ Apply () f x

-- Parses an identifier
ident :: Parser VarName
ident = many1 (noneOf [' '])

-- Parses a variable identifier
varExpr :: Parser (Expr ())
varExpr = Var <$> pure () <*> ident

-- Parses a lambda
lambdaExpr :: Parser (Expr ())
lambdaExpr = do
    _ <- char '\\'
    x <- ident
    spaces
    _ <- string "->"
    spaces
    e <- expr
    pure $ Lambda () x e

-- Parses a let expression
letExpr :: Parser (Expr ())
letExpr = do
    _ <- string "let"
    spaces
    x <- ident
    spaces
    _ <- char '='
    spaces
    e <- expr
    spaces
    _ <- string "in"
    spaces
    b <- expr
    pure $ Let () x e b

-- Parses an expression
expr :: Parser (Expr ())
expr = litStrExpr <|> litIntExpr <|> applyExpr <|> lambdaExpr <|> letExpr <|> varExpr
