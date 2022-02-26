module Main where

import Control.Monad.IO.Class
import Data.Char
import Help
import Lexer
import Parser
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    runInputT,
  )
import System.IO
import Token

license =
  "\n\tfungsi - a functional programming language for simple math calculations\n"
    ++ "\tCopyright (C) 2021  Stasenko Konstantin\n"
    ++ "\n\n"
    ++ "\tThis program is free software: you can redistribute it and/or modify\n"
    ++ "\tit under the terms of the GNU General Public License as published by\n"
    ++ "\tthe Free Software Foundation, either version 3 of the License, or\n"
    ++ "\t(at your option) any later version.\n"
    ++ "\n\n"
    ++ "\tThis program is distributed in the hope that it will be useful,\n"
    ++ "\tbut WITHOUT ANY WARRANTY; without even the implied warranty of\n"
    ++ "\tMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
    ++ "\tGNU General Public License for more details.\n"
    ++ "\n\n"
    ++ "\tYou should have received a copy of the GNU General Public License\n"
    ++ "\talong with this program.  If not, see <http://www.gnu.org/licenses/>.\n"
    ++ "\n\n"
    ++ "\tcontacts:\n"
    ++ "\t    mail   - stasenko.ky@gmail.com\n"
    ++ "\t    github - Stasenko-Konstantin\n\n"

main :: IO ()
main = do
  putStrLn "\t       Fungsi Copyright (C) 2021  Stasenko Konstantin"
  putStrLn "\t       This program comes with ABSOLUTELY NO WARRANTY."
  putStrLn "\tThis is free software, and you are welcome to redistribute it"
  putStrLn "\t   under certain conditions; type `license' for details.\n"
  runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  return $ putStr "< "
  line <- getInputLine "< "
  iLine <- case line of
    Nothing -> return ""
    Just iLine -> return iLine :: InputT IO String
  let isValid = isValidParnts iLine
  let sndValid = show $ snd3 isValid
  let thdValid = show $ thd3 isValid
  if fst3 isValid
    then case iLine of
      "quit" -> return ()
      "license" -> liftIO $ putStrLn license
      _ -> do
        return $ putStr "> "
        tokens <- liftIO $ return (scan $ map toLower iLine)
        liftIO $ print tokens
        let exprs = parse tokens
        liftIO $ print exprs
        repl
    else
      error $
        "Syntax error: missing closing parenthesis, line = "
          ++ sndValid
          ++ ", n = "
          ++ thdValid