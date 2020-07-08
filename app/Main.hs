module Main where

import System.Environment
import Tip.Frontend.Check.TypeCheck (typeCheck)
import Tip.Frontend.Parse.Expr (parseExpr)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inFile:outFile:_) -> do
            source <- readFile inFile
            case parseExpr inFile source of
                Left e -> error e
                Right expr -> writeFile outFile $ show $ typeCheck expr
        _ -> error "Syntax: [input file] [output file]"
