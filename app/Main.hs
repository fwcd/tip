module Main where

import System.Environment
import Tip.Frontend.Utils.Pretty (pretty)
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
                Right expr -> do
                    let ast = typeCheck expr
                    putStrLn $ pretty ast
                    writeFile outFile $ show ast
        _ -> error "Syntax: [input file] [output file]"
