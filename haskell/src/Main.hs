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
    if line == ":q" 
       then return ()
       else do
           putStr "> "
           tokens <- return (scan $ map toLower line)
           putStrLn $ show tokens
           main