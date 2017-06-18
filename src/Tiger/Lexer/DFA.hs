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


-- |Represents deterministic finite automata (DFAs) for use in lexing.
-- Inspired by a similar implementation from Daniele Micciancio,
-- <https://cseweb.ucsd.edu/classes/wi14/cse105-a/haskell/intro.html>.
module Tiger.Lexer.DFA where

-- |Represents possible states of DFAs
data State = Interim Int -- ^state is an intermediate state
           | Final Token -- ^state is a final accepting state
           | NoState -- ^no matches so machine in error state
           deriving Eq
           
-- |DFA
type DFA st = ([st], [Char], st -> Char -> st, st, st -> Bool)

type ConfigDFA st = (st, String)

initConfigDFA :: DFA st -> String -> ConfigDFA st
initConfigDFA (qs,sigma,delta,s,inF) w = (s,w)

nextConfigDFA :: DFA st -> ConfigDFA st -> Maybe (ConfigDFA st)
nextConfigDFA (qs,sigma,delta,s,inF) (q,[]) = Nothing
nextConfigDFA (qs,sigma,delta,s,inF) (q,a:w) = Just (delta q a, w)

runDFA :: DFA st -> String -> ConfigDFA st
runDFA dfa w = run (initConfigDFA dfa w) where
    run conf = case (nextConfigDFA dfa conf) of
                    Nothing -> conf
                    Just newConf -> run newConf
                    
acceptConfigDFA :: DFA st -> ConfigDFA st -> Bool
acceptConfigDFA (qs,sigma,delta,s,inF) (q,[]) = inF q
acceptConfigDFA _ _ = False

execDFA :: DFA st -> String -> Bool
execDFA dfa w = acceptConfigDFA dfa (runDFA dfa w)


