module Main where

import Lexer
import Token

import System.IO
import Data.Char

main :: IO ()
main = do 
    putStr "< "
    hFlush stdout
    line <- getLine
    case line of
        "quit()" -> return ()
        _   -> do
            putStr "> "
            tokens <- return (scan $ map toLower line)
            --exprs  <- return (parse tokens)
            putStrLn $ show tokens --exprs
            main