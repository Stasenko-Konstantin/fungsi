module Main where

import Lexer
import Token
import Expr
import Parser

import System.IO
import Data.Char

main :: IO ()
main = do 
    putStr "< "
    hFlush stdout
    line <- getLine
    if line == ":q" 
       then return ()
       else do
           putStr "> "
           tokens <- return (scan $ map toLower line)
           exprs  <- return (parse tokens)
           putStrLn $ show exprs
           main