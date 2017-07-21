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

-- |Implement DFA-based tokenizer for Tiger language, based on
-- hand-written RegEx -> NFA -> DFA conversion.
module Tiger.Lexer.Tokenizer where

import Data.Char
import Control.Monad
import Data.Set (Set, empty, singleton, union, insert, member, notMember)

import Tiger.Lexer.Tokens (Token(..))
import Tiger.Lexer.DFA (DFA)

-- |State for DFA that keeps track of currently recognized Token
newtype TokenState = TokenState Token
  deriving (Eq, Show)

-- |Tiger Tokenizer
tigerTokenizer = (tigerStates, alphabet, tigerDelta, TokenState BOF, tigerinF) :: DFA TokenState

-- states aren't really used in DFA algorithm, so ignore.
tigerStates = [TokenState BOF]
               
alphabet = identifiers ++ ops ++ whitespace
identifiers = letters ++ digits ++ ['_']
letters = lower ++ upper
lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits = "0123456789"
ops = "+-*/&|.,:;<>=(){}[]"
whitespace = " \n\t"
 
tigerinF (TokenState BOF) = False
tigerinF _ = True

tigerDelta (TokenState BOF) c
    | c `elem` letters = TokenState (ID [c])
    | c `elem` digits = TokenState (NUM ((read [c]) :: Int))
    | c `elem` whitespace = TokenState BOF -- ignore whitespace
    | c == '+' = TokenState (PLUS)
    | c == '-' = TokenState (MINUS)
    | c == '*' = TokenState (STAR)
    | c == '/' = TokenState (SLASH)
    | otherwise = error "Couldn't tokenize input"
tigerDelta (TokenState (ID "arra")) 'y' = TokenState (ARRAY)
tigerDelta (TokenState (ARRAY)) c
    | c `elem` identifiers = TokenState (ID ("array"++[c]))
tigerDelta (TokenState (ID "brea")) 'k' = TokenState (BREAK)
tigerDelta (TokenState (BREAK)) c
    | c `elem` identifiers = TokenState (ID ("break"++[c]))
tigerDelta (TokenState (ID "d")) 'o' = TokenState (DO)
tigerDelta (TokenState (DO)) c
    | c `elem` identifiers = TokenState (ID ("do"++[c]))
tigerDelta (TokenState (ID i)) c
    | c `elem` identifiers = TokenState (ID (i++[c]))



