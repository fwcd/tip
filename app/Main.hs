module Main where

import qualified Data.Text.IO as TIO
import Prettyprinter.Render.Text (hPutDoc, putDoc)
import System.Environment
import System.IO (withFile, IOMode (..))
import Tip.Frontend.AbstractHaskell.Parse (parseExpr)
import Tip.Frontend.Check.TypeCheck (typeCheck)
import Tip.Utils.General
import Tip.Utils.Pretty

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inFile:outFile:_) -> do
            source <- TIO.readFile inFile
            let ast = fromRight $ parseExpr inFile source
                typedAST = typeCheck ast

            putDoc $ prettyPrec 0 typedAST
            putStrLn ""

            withFile outFile WriteMode $ flip hPutDoc $ prettyPrec 0 typedAST
        _ -> error "Syntax: [input file] [output file]"
