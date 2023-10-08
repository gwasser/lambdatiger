{-
    Copyright (C) 2017, Garret Wassermann.

    This file is part of tigerc, the Tiger language compiler,
    based on the Tiger language in "Modern Compiler Implementation
    in ML" by Andrew W. Appel.

    tigerc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    tigerc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with tigerc.  If not, see <http://www.gnu.org/licenses/>.
-}


module Main where

-- tigerc modules
import Tiger.Parser.Grammar (parseExpr)
import Tiger.REPL.Interpreter (eval)
import Tiger.REPL.Pretty (ppexpr)

-- base modules
import System.Environment
import System.Exit
import System.IO

import Data.Version ( showVersion )
import Paths_lambdatiger ( version )

import Control.Monad.Trans
import System.Console.Haskeline
    
process :: Bool -> String -> IO ()
process ast line = do
    let res = parseExpr line
    case res of
         Left err -> print err
         Right ex -> do 
             if ast then print ex else return ()
             case eval ex of
                  Nothing -> putStrLn "Error Evaluating"
                  Just result -> print $ ppexpr result
                  
repl :: Bool -> IO ()
repl ast = runInputT defaultSettings loop
  where
  loop = do
    minput <- getInputLine "tiger> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process ast input) >> loop

main :: IO ()
main = getArgs >>= parseArgs

parseArgs ["-h"]         = usage   >> exit
parseArgs ["--help"]     = usage   >> exit
parseArgs ["-v"]         = version >> exit
parseArgs ["--version"]  = version >> exit
parseArgs ["--show-ast"] = version >> ctrld >> repl True
parseArgs []             = version >> ctrld >> repl False
--parseArgs fs     = concat `fmap` mapM readFile fs
parseArgs _              = error "Error parsing command line arguments"

usage   = putStrLn "Usage: tigeri [-vh]"
version = putStrLn ("Tiger interpreter, version " ++ (showVersion version))
ctrld   = putStrLn "Type CTRL+D to exit"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
