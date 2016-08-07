module Main where

import AST (Assoc)
import ExprParser (exprParser)
import Lexer (scanTokens)
import Parser (opsParser)

import Control.Monad (unless, when)
import Data.Foldable (foldlM)
import Data.List (nub)

import System.Exit (exitWith, ExitCode(..))
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
        [file] <- getArgs
        ops <- readFile file >>= parseOps
        console ops
        putStrLn "done"
    where
        parseOps :: String -> IO [Assoc]
        parseOps opsStr = case opsParser (scanTokens opsStr) of
                Left err  -> print err >> exitWith (ExitFailure 1)
                Right ops -> mapM_ print ops >> return ops

console :: [Assoc] -> IO ()
console ops = untilM isEOF $ do
        lne <- getLine
        let tks = scanTokens lne

        case exprParser ops tks of
                Left err -> print err
                Right ex -> print ex

untilM :: Monad m => m Bool -> m a -> m ()
untilM guard act = do
        bool <- guard
        unless bool (act >> untilM guard act)
