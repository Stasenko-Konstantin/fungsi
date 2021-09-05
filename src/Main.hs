module Main where

import Lexer
import Token
import Parser

import System.IO
import Data.Char

license = "\n\tFungsi - a functional programming language for simple math calculations\n" ++
          "\tCopyright (C) 2021  Stasenko Konstantin\n" ++ "\n\n" ++

          "\tThis program is free software: you can redistribute it and/or modify\n" ++
          "\tit under the terms of the GNU General Public License as published by\n" ++
          "\tthe Free Software Foundation, either version 3 of the License, or\n" ++
          "\t(at your option) any later version.\n" ++ "\n\n" ++

          "\tThis program is distributed in the hope that it will be useful,\n" ++
          "\tbut WITHOUT ANY WARRANTY; without even the implied warranty of\n" ++
          "\tMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" ++
          "\tGNU General Public License for more details.\n" ++ "\n\n" ++

          "\tYou should have received a copy of the GNU General Public License\n" ++
          "\talong with this program.  If not, see <http://www.gnu.org/licenses/>.\n" ++ "\n\n" ++

          "\tcontacts:\n" ++
          "\t    mail   - stasenko.ky@gmail.com\n" ++
          "\t    github - Stasenko-Konstantin\n\n"


main :: IO ()
main = do
    putStrLn "\t       Fungsi Copyright (C) 2021  Stasenko Konstantin"
    putStrLn "\t       This program comes with ABSOLUTELY NO WARRANTY."
    putStrLn "\tThis is free software, and you are welcome to redistribute it"
    putStrLn "\t   under certain conditions; type `license()' for details.\n"
    repl


repl :: IO ()
repl = do 
    putStr "< "
    hFlush stdout
    line <- getLine
    case line of
        "quit()"  -> return ()
        "license()" -> putStrLn license
        _   -> do
            putStr "> "
            tokens <- return (scan $ map toLower line)
            exprs  <- return (parse tokens)
            putStrLn $ show tokens --exprs
            repl