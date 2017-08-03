{
--    Copyright (C) 2017, Garret Wassermann.
--
--    This file is part of tigerc, the Tiger language compiler,
--    based on the Tiger language in "Modern Compiler Implementation
--    in ML" by Andrew W. Appel.
--
--    tigerc is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    tigerc is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with tigerc.  If not, see <http://www.gnu.org/licenses/>.

-- exports alexScanTokens :: String -> [Token]
module Tiger.Lexical.Lexer (alexScanTokens) where 
import Tiger.Lexical.Tokens (Token(..))
}

%wrapper "basic"

$digit = 0-9			            -- digits
$alpha = [a-zA-Z]		            -- alphabetic characters

tokens :-

  $white+               ;
  "/*".*"*/"            ;
  array                 { \s -> ARRAY }
  break                 { \s -> BREAK }
  do                    { \s -> DO }
  else                  { \s -> ELSE }
  end                   { \s -> END }
  for                   { \s -> FOR }
  function              { \s -> FUNCTION }
  if                    { \s -> IF }
  in                    { \s -> IN }
  let                   { \s -> LET }
  nil                   { \s -> NIL }
  of                    { \s -> OF }
  to                    { \s -> TO }
  var                   { \s -> VAR }
  then                  { \s -> THEN }
  type                  { \s -> TYPE }
  while                 { \s -> WHILE }
  
  \+                    { \s -> PLUS }
  \-                    { \s -> MINUS }
  \*                    { \s -> STAR }
  \/                    { \s -> SLASH }
  \&                    { \s -> AMPERSAND }
  \|                    { \s -> PIPE }
  
  \:\=                  { \s -> DEFINE }
  
  \=                    { \s -> EQUAL }
  \<\>                  { \s -> NOTEQUAL }
  \>\=                  { \s -> GREATEROREQUAL }
  \<\=                  { \s -> LESSOREQUAL }
  \>                    { \s -> GREATERTHAN }
  \<                    { \s -> LESSTHAN }
  
  \;                    { \s -> SEMICOLON }
  \:                    { \s -> COLON }
  \.                    { \s -> PERIOD }
  \,                    { \s -> COMMA }
  \{                    { \s -> LBRACE }
  \}                    { \s -> RBRACE }
  \(                    { \s -> LPAREN }
  \)                    { \s -> RPAREN }
  \[                    { \s -> LBRACKET }
  \]                    { \s -> RBRACKET }
  
  $digit+                       { \s -> NUM (read s :: Int) }
  $alpha [$alpha $digit \_]*    { \s -> ID s }
  \"([^\\\"]|\\.|$white+)*\"    { \s -> STR (init $ tail s) }

{
-- Each action has type :: String -> Token
}
