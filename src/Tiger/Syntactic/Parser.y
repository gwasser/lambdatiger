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
module Tiger.Syntactic.Parser (happyTokenParse) where 
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntactic.AST
}


%name happyTokenParse
%tokentype { Token }
%error { parseError }

%right IN OF ELSE
%left '|'
%left '&'
%nonassoc '>' '<' '>=' '<=' '=' '<>'
%left '+' '-'
%left '*' '/'
%left NEG

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
      STR             { STR $$ }

%%

program     : exp                                                   { Program $1 }

exp         : NIL                                                   { NilExp }
            | lvalue                                                { VarExp $1 }
            | NUM                                                   { IntExp $1 }
            | STR                                                   { StrExp $1 }
            | ID '(' ')'                                            { CallExp ($1 :: Symbol) [] }
            | ID '(' explist ')'                                    { CallExp ($1 :: Symbol) $3 }
            | exp '|' exp                                           { IfExp $1 (IntExp 1) (Just $3) }
            | exp '&' exp                                           { IfExp $1 $3 (Just (IntExp 0)) }
            | exp '+' exp                                           { OpExp $1 Add $3 }
            | exp '-' exp                                           { OpExp $1 Sub $3 }
            | exp '*' exp                                           { OpExp $1 Mul $3 }
            | exp '/' exp                                           { OpExp $1 Div $3 }
            | '-' exp %prec NEG                                     { OpExp (IntExp 0) Sub $2 }
            | exp '=' exp                                           { OpExp $1 Equal $3 }
            | exp '<>' exp                                          { OpExp $1 NotEqual $3 }
            | exp '>' exp                                           { OpExp $1 GreaterThan $3 }
            | exp '<' exp                                           { OpExp $1 LessThan $3 }
            | exp '>=' exp                                          { OpExp $1 GreaterEqual $3 }
            | exp '<=' exp                                          { OpExp $1 LessEqual $3 }
            | ID '{' '}'                                            { RecordExp [] ($1 :: Symbol) }
            | ID '{' recflist '}'                                   { RecordExp $3 ($1 :: Symbol) }
            | expseq                                                { SeqExp $1 }
            | lvalue ':=' exp                                       { AssignExp $1 $3 }
            | IF exp THEN exp ELSE exp                              { IfExp $2 $4 (Just $6) }
            | IF exp THEN exp                                       { IfExp $2 $4 Nothing }
            | WHILE exp DO exp                                      { WhileExp $2 $4 }
            | FOR ID ':=' exp TO exp DO exp                         { ForExp ($2 :: Symbol) True $4 $6 $8 }
            | BREAK                                                 { BreakExp }
            | LET decllist IN exp END                               { LetExp $2 $4 }
            | ID '[' exp ']' OF exp                                 { ArrayExp ($1 :: Symbol) $3 $6 }
            | '(' exp ')'                                           { $2 }
            
explist     : exp                                                   { [$1] }
            | exp ',' explist                                       { $1 : $3 } 
            
expseq      : exp                                                   { [$1] }
            | exp ';' expseq                                        { $1 : $3 }
            
lvalue      : ID                                                    { SimpleVar ($1 :: Symbol) }
            | lvalue '.' ID                                         { FieldVar $1 ($3 :: Symbol) }
            | lvalue '[' exp ']'                                    { SubscriptVar $1 $3 }
      
decllist    : decl                                                  { [$1] }
            | decl ',' decllist                                     { $1 : $3 } 
         
decl        : TYPE ID '=' ty                                        { TypeDecl ($2 :: Symbol) $4 }
            | vardecl                                               { $1 }
            | fundecllist                                           { FunDecls $1 }

ty          : ID                                                    { NameType ($1 :: Symbol) }
            | '{' '}'                                               { RecordType [] }
            | '{' fieldlist '}'                                     { RecordType $2 }
            | ARRAY OF ID                                           { ArrayType ($3 :: Symbol) }
            
recflist    : recfield                                              { [$1] }
            | recfield ',' recflist                                 { $1 : $3 }
            
recfield    : ID '=' exp                                            { ($1 :: Symbol,$3) }
   
fieldlist   : field                                                 { [$1] }
            | field ',' fieldlist                                   { $1 : $3 }
            
field       : ID ':' ID                                             { Field ($1 :: Symbol) True ($3 :: Symbol) }
         
vardecl     : VAR ID ':=' exp                                       { VarDecl ($2 :: Symbol) True Nothing $4 }
            | VAR ID ':' ID ':=' exp                                { VarDecl ($2 :: Symbol) True (Just ($4 :: Symbol)) $6 }
            
fundecllist : fundecl                                               { [$1] }
            | fundecl ',' fundecllist                               { $1 : $3 } 
        
fundecl     : FUNCTION ID '(' ')' '=' exp                           { FunDecl ($2 :: Symbol) [] Nothing $6 }
            | FUNCTION ID '(' fieldlist ')' '=' exp                 { FunDecl ($2 :: Symbol) $4 Nothing $7 }
            | FUNCTION ID '(' ')' ':' ID '=' exp                    { FunDecl ($2 :: Symbol) [] (Just ($6 :: Symbol)) $8 }
            | FUNCTION ID '(' fieldlist ')' ':' ID '=' exp          { FunDecl ($2 :: Symbol) $4 (Just ($7 :: Symbol)) $9 }
     
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Could include types representing syntax tree nodes,
-- but we already imported that above from:
-- Tiger.SyntaxAnalysis.Syntax

-- We also already included the lexical tokens from:
-- Tiger.LexicalAnalysis.Tokens
}
