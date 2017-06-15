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


-- |Represents non-deterministic finite automata (NFAs) for use in lexing,
-- based on Chapter 3 of _Compilers: Principles, Techniques, Tools_ 
-- by Aho et al.
module Tiger.Lexer.NFA where

import Tiger.Lexer.RegEx (Regex(..), (||), (**), strToRegexCharSet)
import Tiger.Lexer.Tokens (Token(..))

-- |A Symbol is a character, or the empty symbol (empty string)
type Symbol = Char
data SymbolOrEmpty = Symbol Char | Empty
    deriving (Eq, Show)
-- |An Alphabet is set of accepted symbols
type Alphabet = [Symbol]

-- |Set of accepted input symbols forming input alphabet
stdAlphabet :: Alphabet
stdAlphabet = upper ++ lower ++ underscore ++ ops ++ grouping
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lower = "abcdefghijklmnopqrstuvwxyz"
underscore = "_"
ops = "+-*/%=><.,:;|&"
grouping = "()[]{}"

-- |Represents possible states of NFAs
data State = Interim Int -- ^state is an intermediate state
           | Final Int -- ^state is a final accepting state
           | Initial Int -- ^initial starting state of the NFA
           deriving (Eq, Show)

-- |An NFA is a graph of States to other States where each edge
-- represents a transition upon reading a given a Symbol.
-- The first item (tuple) is the Initial State and Symbol read,
-- and the second item (list) is the list of States that
-- the initial State may transition to when reading given Symbol.
type NFA = [ ((State, SymbolOrEmpty), [State]) ]

-- |Convert a Regex into NFA
regexToNFA :: Regex -> NFA
regexToNFA re = constructSubNFA 0 re
    where constructSubNFA n Epsilon = [(((Initial n), Empty), [(Final (n+1))])]
          constructSubNFA n (Exact a) = [(((Initial n), Symbol a), [(Final (n+1))])]
          constructSubNFA _ _ = undefined
