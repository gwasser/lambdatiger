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
           
-- |DFA is defined by five properties:
-- 1. list of possible states
-- 2. set of accepted input characters (alphabet)
-- 3. a transition function that takes a state and an input symbol
--    and returns a new state
-- 4. a state designated as the initial start state
-- 5. set of final accepting states, here represented by a predicate
--    that returns true if a given state is a final state
-- The DFA is parameterized to wrap any type so any required
-- metadata is included in the state st.
type DFA st = ([st], [Char], st -> Char -> st, st, st -> Bool)

-- |Current configuration of a DFA, consists of
-- current state and the unprocessed remainder of input string
type ConfigDFA st = (st, String)

-- |Create an initial configuration for a DFA based on
-- a given input String
initConfigDFA :: DFA st -> String -> ConfigDFA st
initConfigDFA (qs,sigma,delta,s,inF) w = (s,w)

-- |Read a character from the input String and return a new
-- configuration representing the state transitioned to,
-- or a Nothing if no transition can be made
nextConfigDFA :: DFA st -> ConfigDFA st -> Maybe (ConfigDFA st)
nextConfigDFA (qs,sigma,delta,s,inF) (q,[]) = Nothing
nextConfigDFA (qs,sigma,delta,s,inF) (q,a:w) = Just (delta q a, w)

-- |Given a DFA and an input String, returns either a final
-- state or an intermediate state (possibly the initial state)
-- depending whether the input String was accepted or not
runDFA :: DFA st -> String -> ConfigDFA st
runDFA dfa w = run (initConfigDFA dfa w) where
    run conf = case (nextConfigDFA dfa conf) of
                    Nothing -> conf
                    Just newConf -> run newConf
     
-- |Return True if given configuration of a DFA is a final state
acceptConfigDFA :: DFA st -> ConfigDFA st -> Bool
acceptConfigDFA (qs,sigma,delta,s,inF) (q,[]) = inF q

-- |Return True if a given input String is accepted by a given DFA
execDFA :: DFA st -> String -> Bool
execDFA dfa w = acceptConfigDFA dfa (runDFA dfa w)


