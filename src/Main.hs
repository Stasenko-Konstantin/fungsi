module Main where

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
           putStrLn line