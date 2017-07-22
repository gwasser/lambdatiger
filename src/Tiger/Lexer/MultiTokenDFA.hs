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

-- |Wrapper around DFA implementation to allow processing
-- a stream of multiple input tokens
module Tiger.Lexer.MultiTokenDFA where

import Data.Char
import Control.Monad
import Data.Set (Set, empty, singleton, union, insert, member, notMember)

import Tiger.Lexer.DFA (DFA, StateConfig(..), initStateConfig, acceptStateConfig, nextStateConfig, execDFA, runDFA)

insertCharIntoStateConfig :: StateConfig st -> String -> StateConfig st
insertCharIntoStateConfig (StateConfig {state=state, input=input}) (c:w)
    =  StateConfig {state=state, input=c:input}
                      
-- we run the DFA on the input String one character at a time,
-- until we can't recognize anything anymore, then return the
-- previously accepted Token and remainding of input
runMultiTokenDFA :: DFA st -> String -> StateConfig st
runMultiTokenDFA dfa [] = error "No input stream provided"
runMultiTokenDFA dfa (c:w) = run (initStateConfig dfa [c]) undefined w where
    run conf prevconf [] = if acceptStateConfig dfa conf then conf else prevconf
    run conf prevconf remainder = case (nextStateConfig dfa conf) of
                    Just newConf -> run newConf conf remainder
                    -- ran out of chars, see if final state
                    Nothing -> if acceptStateConfig dfa conf
                                  then run (insertCharIntoStateConfig conf remainder) conf (tail remainder)
                                  else prevconf
          




