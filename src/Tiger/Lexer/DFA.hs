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


-- |Represents deterministic finite automata (DFAs) for use in lexing
module Tiger.Lexer.DFA where

import Tiger.Lexer.RegEx (RegEx(..), (||), (**), strToRECharSet)
import Tiger.Lexer.Tokens (Token(..), Position)

-- |Represents possible states of DFAs
data State = Interim Int -- ^state is an intermediate state
           | Final Token -- ^state is a final accepting state
           | NoState -- ^no matches so machine in error state
           deriving Eq
           
-- |Transitions occur to another State when Regex is matched
-- (Regex should be single character or empty).
type Transition = (Regex, State)

-- |A DFA is represented by list of all possible transitions;
-- rule priority is implemented by putting higher priority
-- rules earlier in this list
type DFA = [Transition]

-- |Translate a regex into a 
regexToDFA :: Regex -> DFA
           
-- |Applying DFA rules to a Char input when DFA is in given State,
-- results in a new State
transition :: Char -> DFA -> State -> State
transition c d s = if (c == 
