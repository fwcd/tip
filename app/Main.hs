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
            writeFile outFile $ show expr
        _ -> error "Syntax: [input file] [output file]"
