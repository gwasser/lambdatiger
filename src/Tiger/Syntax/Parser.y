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

-- exports happyTokenParse :: [Token] -> Program
module Tiger.Syntax.Parser (happyTokenParse) where 
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntax.AST
}


%name happyTokenParse
%tokentype { Token }
%error { parseError }

%token 
      ARRAY           { ARRAY }
      BREAK           { BREAK }
      DO              { DO }
      ELSE            { ELSE }
      END             { END }
      FOR             { FOR }
      FUNCTION        { FUNCTION }
      IF              { IF }
      IN              { IN }
      LET             { LET }
      NIL             { NIL }
      OF              { OF }
      TO              { TO }
      VAR             { VAR }
      THEN            { THEN }
      TYPE            { TYPE }
      WHILE           { WHILE }
  
      '+'             { PLUS }
      '-'             { MINUS }
      '*'             { STAR }
      '/'             { SLASH }
      '&'             { AMPERSAND }
      '|'             { PIPE }
  
      ':='            { DEFINE }
  
      '='             { EQUAL }
      '<>'            { NOTEQUAL }
      '>='            { GREATEROREQUAL }
      '<='            { LESSOREQUAL }
      '>'             { GREATERTHAN }
      '<'             { LESSTHAN }
  
      ';'             { SEMICOLON }
      ':'             { COLON }
      '.'             { PERIOD }
      ','             { COMMA }
      '{'             { LBRACE }
      '}'             { RBRACE }
      '('             { LPAREN }
      ')'             { RPAREN }
      '['             { LBRACKET }
      ']'             { RBRACKET }
      
      NUM             { NUM $$ }
      ID              { ID $$ }

%%

program     : exp                                                   { Program $1 }

exp         : NIL                                                   { Nil }
            | ID '[' exp ']' OF exp                                 { AssignArray $1 $3 $6 }
            | lvalue ':=' exp                                       { AssignVar $1 $3 }
            | IF exp THEN exp ELSE exp                              { IfElse $2 $4 $6 }
            | IF exp THEN exp                                       { If $2 $4 }
            | WHILE exp DO exp                                      { While $2 $4 }
            | FOR ID ':=' exp TO exp DO exp                         { For $2 $4 $6 $8 }
            | BREAK                                                 { Break }
            | LET decllist IN exp END                               { Let $2 $4 }
            | arithexp                                              { ArithExp $1 }
            | comparison                                            { CompareExp $1 }
            | boolean                                               { BoolExp $1 }
            | '(' exp ')'                                           { Exp $2 }
      
decllist    : decl                                                  { Decl $1 }
            | decllist ',' decl                                     { List $1 $3 } 
         
decl        : TYPE ID '=' ty                                        { TyDecl $2 $4 }
            | vardecl                                               { VarDecl $1 }
            | fundecl                                               { FunDecl $1 }

ty          : ID                                                    { TypeName $1 }
            | tyfieldlist                                           { TypeFields $1 }
            | ARRAY OF ID                                           { TypeArray $3 }
   
tyfieldlist :                                                       { Empty }
            | ID ':' ID                                             { Field $1 $3 }
            | ID ':' ID ',' tyfieldlist                             { FieldList $1 $3 $5 }
         
vardecl     : VAR ID ':=' exp                                       { InitVar $2 $4 }
            | VAR ID ':' ID ':=' exp                                { InitVarType $2 $4 $6 }
        
fundecl     : FUNCTION ID '(' tyfieldlist ')' '=' exp               { FunDef $2 $4 $7 }
            | FUNCTION ID '(' tyfieldlist ')' ':' ID '=' exp        { FunDefType $2 $4 $7 $9 }
        
lvalue      : ID                                                    { VarVal $1 }
            | lvalue '.' ID                                         { FieldVal $1 $3 }
            | lvalue '[' exp ']'                                    { ArrayVal $1 $3 }
         
arithexp    : arithexp '+' term                                     { Add $1 $3 }
            | arithexp '-' term                                     { Sub $1 $3 }
            | term                                                  { Term $1 }
         
term        : term '*' factor                                       { Mul $1 $3 }
            | term '/' factor                                       { Div $1 $3 }
            | factor                                                { Factor $1 }
     
factor      : NUM                                                   { Int $1 }
            | '-' NUM                                               { Sub (Int 0) (Int $2) }
            | ID                                                    { Var $1 }
            | '(' exp ')'                                           { AParen $2 }

comparison  : exp '=' exp                                           { Equal $1 $3 }
            | exp '<>' exp                                          { NotEqual $1 $3 }
            | exp '>' exp                                           { GreaterThan $1 $3 }
            | exp '<' exp                                           { LessThan $1 $3 }
            | exp '>=' exp                                          { GreaterEqual $1 $3 }
            | exp '<=' exp                                          { LessEqual $1 $3 }
            
boolean     : boolean '|' booland                                   { Or $1 $3 }
            | booland                                               { BTerm $1 }
            
booland     : booland '&' exp                                       { And $1 $3 }
            | '(' exp ')'                                           { BParen $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Could include types representing syntax tree nodes,
-- but we already imported that above from:
-- Tiger.SyntaxAnalysis.Syntax

-- We also already included the lexical tokens from:
-- Tiger.LexicalAnalysis.Tokens
}
