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


module Tiger.Lexical.Tokens where

-- |A LexicalToken consists of a Token along with appropriate meta data.
-- |If no metadata (such as end of file reached), then meta is Nothing.
-- Implemented as a record, as suggested in a StackOverflow thread:
-- <https://stackoverflow.com/questions/34847211/getting-line-number-information-in-the-semantic-analysis-phase-using-alex-happy>
data L a = L { getPos :: AlexPosn, unPos :: a } deriving (Eq, Show)

-- |AlexPosn represents the position within the lexical stream for each token.
data AlexPosn = AlexPosn { absolute :: Int, row :: Int, col :: Int } deriving (Eq, Show)

-- |A Token of the Tiger language as read by the lexer.
-- Tokens consist of all keywords, punctuation, and data types
-- of the Tiger language, as well as special End of File (EOF) token.
-- Data type tokens have an additional constructor argument
-- representing the value of the token.
data Token = TEOF
           -- keywords
           | ARRAY 
           | BREAK 
           | DO 
           | ELSE 
           | END 
           | FOR 
           | FUNCTION
           | IF
           | IN 
           | LET 
           | NIL 
           | OF 
           | TO 
           | VAR 
           | THEN 
           | TYPE
           | WHILE 
           -- operators
           | AMPERSAND 
           | COLON 
           | COMMA 
           | DEFINE  -- :=
           | EQUAL 
           | GREATERTHAN 
           | GREATEROREQUAL 
           | LESSTHAN
           | LESSOREQUAL 
           | MINUS 
           | NOTEQUAL  -- <>
           | PERIOD 
           | PIPE 
           | PLUS 
           | SEMICOLON 
           | SLASH 
           | STAR
           -- identifiers
           | ID String
           -- numbers (integers)
           | NUM Int
           -- string literal
           | STR String
           -- groupings
           | LBRACE 
           | LBRACKET 
           | LPAREN 
           | RBRACE 
           | RBRACKET 
           | RPAREN 
           deriving (Eq, Show)

getStr :: Token -> String
getStr (ID s) = s
getStr (STR s) = s
getStr _ = error "Not an ID or STR Token"

getNum :: Token -> Int
getNum (NUM n) = n
getNum _ = error "Not a NUM Token"
