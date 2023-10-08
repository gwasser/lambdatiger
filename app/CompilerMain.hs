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
--import Tiger.Parser.NanoParsec (someParseFunc)

-- base modules
import System.Environment
import System.Exit

import Data.Version ( showVersion )
import Paths_lambdatiger ( version )

main :: IO ()
main = getArgs >>= parseArgs

parseArgs ["-h"] = usage   >> exit
parseArgs ["-v"] = info    >> exit
--parseArgs []     = print $ getContents
--parseArgs fs     = print $ concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: tigerc [-vh] [file ..]"
info    = putStrLn ("tigerc compiler, version " ++ (showVersion version))
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
