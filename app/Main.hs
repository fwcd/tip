module Main where

import System.Environment
import Tip.Frontend.Parse.Expr

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inFile:outFile:_) -> do
            source <- readFile inFile
            let expr = parseExpr source
            putStrLn $ show expr
        _ -> putStrLn "Syntax: [input file] [output file]"
