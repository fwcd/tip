module Main where

import Data.Either.Combinators (fromRight')
import qualified Data.Text as T
import System.Environment
import Tip.Frontend.AbstractHaskell.Parse (parseExpr)
import Tip.Frontend.Check.TypeCheck (typeCheck)
import Tip.Utils.Pretty (pretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inFile:outFile:_) -> do
            source <- readFile inFile
            let ast = fromRight' $ parseExpr (T.pack inFile) (T.pack source)
                typedAST = typeCheck ast
            putStrLn $ T.unpack $ pretty typedAST
            writeFile outFile $ show ast
        _ -> error "Syntax: [input file] [output file]"
