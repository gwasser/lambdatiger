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
module Tiger.Lexer.RegEx where

data Regex = REChar Char -- |a single character
           | Empty -- |epsilon, or the empty string
           | Alternate Regex Regex -- |A|B, match either A or B, for two regexes A and B
           | Concat Regex Regex -- |AB, A followed by B, for two regexes A and B
           | Repeat0 Regex -- |*, zero or more occurrences of a regex
           | Repeat1 Regex -- |+, one or more occurrences of a regex
           | Optional Regex -- |?, zero or one occurence of a regex
           | Set String -- |Set of single characters that are acceptable for a match
           | AnyChar -- |any character matches other than newline
           | Quoted String -- |A literal string in quotes, must match exactly
           
(||) :: Regex -> Regex -> Regex
(||) r1 r2 = Alternate r1 r2

(**) :: Regex -> Regex -> Regex
(**) r1 r2 = Concat r1 r2

strToRECharSet :: String -> [Regex]
strToRECharSet [] = []
strToRECharSet (c:cs) = (REChar c) : (strToRECharSet cs)
