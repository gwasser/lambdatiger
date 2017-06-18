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

-- |Two stage process to convert RE to DFA so we can tokenize input stream.
-- |1. Decorate Regex AST with extra meta info (firstpos, lastpos, followpos)
-- |2. Follow algorithm to convert decorated Regex AST to DFA.
module Tiger.Lexer.Tokenizer where

import Data.Char
import Control.Monad
import Data.Set (Set, empty, singleton, union, insert, member, notMember)

import Tiger.Lexer.Regex (Regex(..), (||), (**), strToRECharSet)
import Tiger.Lexer.Tokens (Token(..))
import Tiger.Lexer.DFA (DFA)


-- |Translate a Regex into a DFA using implementation of
-- algorithm presented in Section 3.9.5 of Aho et al.
regexToDFA :: Regex a -> DFA (Regex a)
regexToDFA = undefined

-- |Node in AST for Regex, represents subregex with metadata
-- to facilitate conversion to DFA.
-- Types represent: the Regex represented, and results
-- of firstpos, lastpos, followpos, on this node, respectively.
newtype Node a = Node (Regex a) (Set (Regex a)) (Set (Regex a)) (Set (Regex a))
type Index = Int

nullable :: Regex a -> Bool
nullable Epsilon = True
nullable (Exact c) = False
nullable (Star r) = True
nullable (Or r1 r2) = nullable(r1) or nullable(r2)
nullable (Cat r1 r2) = nullable(r1) and nullable(r2)

firstpos :: Regex a -> Set (Regex a)
firstpos Epsilon = empty
firstpos (Exact c) = singleton (Exact c)
firstpos (Star r) = firstpos(r)
firstpos (Or r1 r2) = firstpos(r1) `union` firstpos(r2)
firstpos (Cat r1 r2) = if (nullable r1) then firstpos(r1) `union` firstpos(r2) else firstpos(r1)

lastpos :: Regex a -> Set (Regex a)
lastpos Epsilon = empty
lastpos (Exact c) = singleton (Exact c)
lastpos (Star r) = lastpos(r)
lastpos (Or r1 r2) = lastpos(r1) `union` lastpos(r2)
lastpos (Cat r1 r2) = if (nullable r2) then lastpos(r1) `union` lastpos(r2) else lastpos(r2)

-- |Set of subregexes that can follow a given regex;
-- basically returns set of sibling and parent subregexes?
followpos :: Regex a -> Set (Regex a)
followpos (Or _ _) = empty
followpos (Cat _ _) = empty
followpos (Star _) = empty
followpos Epsilon = []
followpos (Exact c) = []
