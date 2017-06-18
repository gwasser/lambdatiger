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


-- |Represents regular expressions in a Haskell type class for use in
-- translating human-readable regexes to DFAs for lexing
module Tiger.Lexer.Regex where

import Tiger.Lexer.DFA (DFA)

-- |Represent a regular expression as an AST (possibly augmented)
data Regex a = Exact Char -- |a single character
           | Epsilon -- |epsilon, or the empty string
           | Or (Regex a) (Regex a) -- |A|B, match either A or B, for two regexes A and B
           | Cat (Regex a) (Regex a) -- |AB, A followed by B, for two regexes A and B
           | Star (Regex a) -- |*, zero or more occurrences of a regex
           | Sentinel a -- |Sentinel marking end of regex to form an augmented regex, with parameterized meta info about accepting state
           deriving Eq
           
(||) :: Regex a -> Regex a -> Regex a
(||) r1 r2 = Or r1 r2

(**) :: Regex a -> Regex a -> Regex a
(**) r1 r2 = Cat r1 r2

strToRegexCharSet :: String -> [Regex a]
strToRegexCharSet [] = []
strToRegexCharSet (c:cs) = (Exact c) : (strToRegexCharSet cs)

-- |Converts a character set into equivalent recursive Ors.
-- Assumes the '[' and ']' have already been removed from String.
charSetToOr :: String -> Regex a
charSetToOr s = foldl Or (head stream) (tail stream)
    where stream = (strToRegexCharSet s)

strToCat :: String -> Regex a
strToCat (c:cs) = Cat (Exact c) (strToCat cs)
          
-- |Two stacks to assist in reading a regex string.
-- First is last expression stack, second is () or [] stack,
-- and String is remainder of regex String to parse.
type RegexState = ([Char], [Char], String, Maybe (Regex a))

initRegexState :: String -> RegexState
initRegexState s = ([],[],s,Nothing)

nextRegexState :: RegexState -> Maybe RegexState
-- if parentheses, keep reading until end parens
nextRegexState (l,[],('(':cs),r) = (l,['('],cs,r)
nextRegexState (l,p,(c:cs),r) =
    case c of
         '|' -> ([],pJust (Or (Exact (reverse l)) (strToRegex cs))

-- |Simple parsing function to read a regex String into Regex AST
strToRegex :: String -> Regex a
strToRegex [] = error "Error reading regex string: ran out of characters"
strToRegex [c] = 
strToRegex (c:cs) = case c of
                         '|' -> Or (Exact c) (strToRegex cs)
