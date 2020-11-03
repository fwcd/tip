module Main where

import Data.Either.Combinators (fromRight')
import qualified Data.Text.IO as TIO
import Prettyprinter (Pretty (..))
import Prettyprinter.Render.Text (hPutDoc, putDoc)
import System.Environment
import System.IO (withFile, IOMode (..))
import Tip.Frontend.AbstractHaskell.Parse (parseExpr)
import Tip.Frontend.Check.TypeCheck (typeCheck)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (inFile:outFile:_) -> do
            source <- TIO.readFile inFile
            let ast = fromRight' $ parseExpr inFile source
                typedAST = typeCheck ast
            putDoc $ pretty typedAST
            withFile outFile WriteMode $ flip hPutDoc $ pretty typedAST
        _ -> error "Syntax: [input file] [output file]"
