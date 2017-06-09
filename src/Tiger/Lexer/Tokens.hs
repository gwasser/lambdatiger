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


module Tiger.Lexer.Tokens where 

-- |Represents position within the input text stream as an integer.
type Position = Int

-- |A Token as read by the lexer. Every token type takes at least
-- two Positions, representing the beginning position of the token
-- and end position of the token, resepectively.
-- Tokens consist of all keywords, punctuation, and data types
-- of the Tiger language, as well as special End of File (EOF) token.
-- Data type tokens have an additional third constructor argument
-- representing the value of the token.
data Token = EOF Position Position
           | ARRAY Position Position
           | BREAK Position Position
           | DO Position Position
           | ELSE Position Position
           | END Position Position
           | FOR Position Position
           | FUNCTION Position Position
           | IF Position Position
           | IN Position Position
           | LET Position Position
           | NIL Position Position
           | OF Position Position
           | TO Position Position
           | VAR Position Position
           | THEN Position Position
           | TYPE Position Position
           | WHILE Position Position
           | AMPERSAND Position Position
           | COLON Position Position
           | COMMA Position Position
           | DEFINE Position Position -- :=
           | EQUAL Position Position
           | GREATERTHAN Position Position
           | GREATEROREQUAL Position Position
           | LBRACE Position Position
           | LBRACKET Position Position
           | LESSTHAN Position Position
           | LESSOREQUAL Position Position
           | LPAREN Position Position
           | MINUS Position Position
           | NOTEQUAL Position Position -- <>
           | PERIOD Position Position
           | PIPE Position Position
           | PLUS Position Position
           | RBRACE Position Position
           | RBRACKET Position Position
           | RPAREN Position Position
           | SEMICOLON Position Position
           | SLASH Position Position
           | STAR Position Position
           | ID String Position Position
           | NUM Int Position Position
