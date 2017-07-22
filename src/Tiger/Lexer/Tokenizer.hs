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
newtype LexerState = LexerState Token
  deriving (Eq, Show)

-- |Tiger Tokenizer
tigerTokenizer = (tigerStates, alphabet, tigerDelta, LexerState BOF, tigerinF) :: DFA LexerState

-- states aren't really used in DFA algorithm, so ignore.
tigerStates = [LexerState BOF]
               
alphabet = identifiers ++ ops ++ whitespace
identifiers = letters ++ digits ++ ['_']
letters = lower ++ upper
lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits = "0123456789"
ops = "+-*/&|.,:;<>=(){}[]"
whitespace = " \n\t"
 
tigerinF (LexerState BOF) = False
tigerinF _ = True

tigerDelta (LexerState BOF) c
    | c `elem` letters = LexerState (ID [c])
    | c `elem` digits = LexerState (NUM ((read [c]) :: Int))
    | c `elem` whitespace = LexerState BOF -- ignore whitespace
    | c == '+' = LexerState (PLUS)
    | c == '-' = LexerState (MINUS)
    | c == '*' = LexerState (STAR)
    | c == '/' = LexerState (SLASH)
    | otherwise = error "Couldn't tokenize input"
tigerDelta (LexerState (ID "arra")) 'y' = LexerState (ARRAY)
tigerDelta (LexerState (ARRAY)) c
    | c `elem` identifiers = LexerState (ID ("array"++[c]))
tigerDelta (LexerState (ID "brea")) 'k' = LexerState (BREAK)
tigerDelta (LexerState (BREAK)) c
    | c `elem` identifiers = LexerState (ID ("break"++[c]))
tigerDelta (LexerState (ID "d")) 'o' = LexerState (DO)
tigerDelta (LexerState (DO)) c
    | c `elem` identifiers = LexerState (ID ("do"++[c]))
tigerDelta (LexerState (ID i)) c
    | c `elem` identifiers = LexerState (ID (i++[c]))



