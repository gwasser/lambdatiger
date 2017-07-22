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
data LexerState = LexerState { current :: Token,
                               line :: Int,
                               col :: Int
                             } deriving (Eq, Show)

-- |Tiger Tokenizer
tigerDFA = (tigerStates, alphabet, tigerDelta, lexerInitState, tigerinF) :: DFA LexerState

lexerInitState = LexerState { current = BOF,
                              line = 0,
                              col = 0
                            }

-- states aren't really used in DFA algorithm, so ignore.
tigerStates = []
               
alphabet = identifiers ++ ops ++ whitespace
identifiers = letters ++ digits ++ ['_']
letters = lower ++ upper
lower = "abcdefghijklmnopqrstuvwxyz"
upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits = "0123456789"
ops = "+-*/&|.,:;<>=(){}[]"
whitespace = spaces ++ newline
spaces = " \t"
newline = "\n"
 
tigerinF (LexerState {current = BOF}) = False
tigerinF (LexerState {current = UNACCEPTED}) = False
tigerinF _ = True

-- from initial state
tigerDelta (LexerState {current=BOF, line=l, col=n}) c
    | c `elem` letters = LexerState {current=ID [c], line=l, col=(n+1)}
    | c `elem` digits = LexerState {current=NUM ((read [c]) :: Int), line=l, col=(n+1)}
    | c `elem` whitespace = LexerState {current=BOF, line=l, col=(n+1)} -- ignore whitespace
    | c == '+' = LexerState {current=PLUS, line=l, col=(n+1)}
    | c == '-' = LexerState {current=MINUS, line=l, col=(n+1)}
    | c == '*' = LexerState {current=STAR, line=l, col=(n+1)}
    | c == '/' = LexerState {current=SLASH, line=l, col=(n+1)}
    | otherwise = error "Couldn't tokenize input"

tigerDelta (LexerState {current=cur, line=l, col=n}) c = LexerState {current=newcur, line=newl, col=newn}
    where newn = n+1
          newl = l
          newcur = if c `elem` alphabet
                      then transition cur c
                      else error "Input not in accepted alphabet"
          
          transition :: Token -> Char -> Token
          transition (ID "arra") 'y' = ARRAY
          transition (ARRAY) c' = ID ("array"++[c'])
          transition (ID "brea") 'k' = BREAK
          transition (BREAK) c' = ID ("break"++[c'])
          transition (ID "d") 'o' = DO
          transition (DO) c' = ID ("do"++[c'])
          transition (ID i) c'
              | c `elem` identifiers = ID (i++[c'])
          transition _ _ = UNACCEPTED
          
          

--tigerDelta (LexerState {current=ID "arra", line=l, col=n}) 'y' = LexerState {current=ARRAY, line=l, col=(n+1)}
--tigerDelta (LexerState {current=(ARRAY), line=l, col=n}) c
--    | c `elem` identifiers = LexerState {current=ID ("array"++[c]), line=l, col=(n+1)}
--tigerDelta (LexerState {current=(ID "brea"), line=l, col=n}) 'k' = LexerState {current=BREAK, line=l, col=(n+1)}
--tigerDelta (LexerState {current=(BREAK), line=l, col=n}) c
--    | c `elem` identifiers = LexerState {current=ID ("break"++[c]), line=l, col=(n+1)}
--tigerDelta (LexerState {current=(ID "d"), line=l, col=n}) 'o' = LexerState {current=DO, line=l, col=(n+1)}
--tigerDelta (LexerState {current=(DO), line=l, col=n}) c
--    | c `elem` identifiers = LexerState {current=ID ("do"++[c]), line=l, col=(n+1)}
--tigerDelta (LexerState {current=(ID i), line=l, col=n}) c
--    | c `elem` identifiers = LexerState {current=ID (i++[c]), line=l, col=(n+1)}



