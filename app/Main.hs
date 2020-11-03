module Main where

import Data.Either.Combinators (fromRight')
import qualified Data.Text.IO as TIO
import System.Environment
import Tip.Frontend.AbstractHaskell.Parse (parseExpr)
import Tip.Frontend.Check.TypeCheck (typeCheck)
import Tip.Utils.Pretty (pretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inFile:outFile:_) -> do
            source <- TIO.readFile inFile
            let ast = fromRight' $ parseExpr inFile source
                typedAST = typeCheck ast
            TIO.putStrLn $ pretty typedAST
            TIO.writeFile outFile $ pretty typedAST
        _ -> error "Syntax: [input file] [output file]"
