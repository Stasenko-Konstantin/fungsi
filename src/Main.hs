module Main where

import Lexer
import Token

import System.IO

main :: IO ()
main = do 
    putStr "< "
    hFlush stdout
    line <- getLine
    if line == ":q" 
       then return ()
       else do
           putStr "> "
           tokens <- return (scan line)
           putStrLn $ show tokens
           main