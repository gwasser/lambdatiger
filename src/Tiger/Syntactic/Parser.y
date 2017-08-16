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
--     and happyTokenParseWithMeta :: [LexicalToken] -> Program
module Tiger.Syntactic.Parser (happyTokenParse) where 

import Control.Monad.Except

import Tiger.Lexical.Tokens (Token(..), LexicalToken, TokenMeta(..))
import Tiger.Syntactic.AST
}


%name happyTokenParse
%tokentype { Token }
%error { parseError }

%nonassoc DO ASSIGN
%right IN OF ELSE
%left '|'
%left '&'
%nonassoc '>' '<' '>=' '<=' '=' '<>'
%left '+' '-'
%left '*' '/'
%left NEG

%token 
      EOF             { TEOF }

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

program     : exp EOF                                               { Program $1 }

exp         : NIL                                                   { NilExp }
            | lvalue                                                { VarExp $1 }
            | NUM                                                   { IntExp $1 }
            | STR                                                   { StrExp $1 }
            | '(' expseq ')'                                        { if length($2) == 1 then head $2 else SeqExp (reverse $2) }
            | '-' exp %prec NEG                                     { OpExp (IntExp 0) Sub $2 }
            | ID '(' arglist ')'                                    { CallExp ($1 :: Symbol) (reverse $3) }
            | exp '*' exp                                           { OpExp $1 Mul $3 }
            | exp '/' exp                                           { OpExp $1 Div $3 }
            | exp '+' exp                                           { OpExp $1 Add $3 }
            | exp '-' exp                                           { OpExp $1 Sub $3 }
            | exp '=' exp                                           { OpExp $1 Equal $3 }
            | exp '<>' exp                                          { OpExp $1 NotEqual $3 }
            | exp '>' exp                                           { OpExp $1 GreaterThan $3 }
            | exp '<' exp                                           { OpExp $1 LessThan $3 }
            | exp '>=' exp                                          { OpExp $1 GreaterEqual $3 }
            | exp '<=' exp                                          { OpExp $1 LessEqual $3 }
            | exp '&' exp                                           { IfExp $1 $3 (Just (IntExp 0)) }
            | exp '|' exp                                           { IfExp $1 (IntExp 1) (Just $3) }
            | ID '{' recflist '}'                                   { RecordExp $3 ($1 :: Symbol) }
            | lvalue ':=' exp                                       { AssignExp $1 $3 }
            | IF exp THEN exp ELSE exp                              { IfExp $2 $4 (Just $6) }
            | IF exp THEN exp                                       { IfExp $2 $4 Nothing }
            | WHILE exp DO exp                                      { WhileExp $2 $4 }
            | FOR ID ':=' exp TO exp DO exp                         { ForExp ($2 :: Symbol) True $4 $6 $8 }
            | BREAK                                                 { BreakExp }
            | LET decllist IN expseq END                            { LetExp (reverse $2) (if length($4) == 1 then head $4 else SeqExp (reverse $4)) }
            | ID '[' exp ']' OF exp                                 { ArrayExp ($1 :: Symbol)$3 $6 }
            
expseq      : {- empty -}                                           { [] }
            | expseq ';' exp                                        { $3 : $1 } 
            | exp                                                   { [$1] }
            
arglist     : {- empty -}                                           { [] }
            | arglist ',' exp                                       { $3 : $1 } 
            | exp                                                   { [$1] }
            
lvalue      : ID                                                    { SimpleVar ($1 :: Symbol) }
            | lvalue '.' ID                                         { FieldVar $1 ($3 :: Symbol) }
            | ID '[' exp ']'                                        { SubscriptVar (SimpleVar ($1 :: Symbol)) $3 }
            | lvalue '[' exp ']'                                    { SubscriptVar $1 $3 }
      
decllist    : {- empty -}                                           { [] }
            | decllist decl                                         { $2 : $1 } 
         
decl        : TYPE ID '=' ty                                        { TypeDecl ($2 :: Symbol) $4 }
            | vardecl                                               { $1 }
            | fundecllist                                           { FunDecls (reverse $1) }

ty          : ID                                                    { NameType ($1 :: Symbol) }
            | '{' '}'                                               { RecordType [] }
            | '{' fieldlist '}'                                     { RecordType $2 }
            | ARRAY OF ID                                           { ArrayType ($3 :: Symbol) }
            
recflist    : recfield                                              { [$1] }
            | recflist ',' recfield                                 { $3 : $1 }
            | {- empty -}                                           { [] }
            
recfield    : ID '=' exp                                            { ($1 :: Symbol,$3) }
   
fieldlist   : field                                                 { [$1] }
            | field ',' fieldlist                                   { $1 : $3 }
            
field       : ID ':' ID                                             { Field ($1 :: Symbol) True ($3 :: Symbol) }
         
vardecl     : VAR ID ':=' exp                                       { VarDecl ($2 :: Symbol) True Nothing $4 }
            | VAR ID ':' ID ':=' exp                                { VarDecl ($2 :: Symbol) True (Just ($4 :: Symbol)) $6 }
            
fundecllist : {- empty -}                                           { [] }
            | fundecllist fundecl                                   { $2 : $1 } 
        
fundecl     : FUNCTION ID '(' ')' '=' exp                           { FunDecl ($2 :: Symbol) [] Nothing $6 }
            | FUNCTION ID '(' fieldlist ')' '=' exp                 { FunDecl ($2 :: Symbol) $4 Nothing $7 }
            | FUNCTION ID '(' ')' ':' ID '=' exp                    { FunDecl ($2 :: Symbol) [] (Just ($6 :: Symbol)) $8 }
            | FUNCTION ID '(' fieldlist ')' ':' ID '=' exp          { FunDecl ($2 :: Symbol) $4 (Just ($7 :: Symbol)) $9 }
     
{

-- |Convenience function used in testing, ignores metadata
--happyTokenParse :: [Token] -> Program
--happyTokenParse ts = happyTokenParseWithMeta $ map (\t -> (t,Nothing)) ts

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Could include types representing syntax tree nodes,
-- but we already imported that above from:
-- Tiger.Syntactic.Syntax

-- We also already included the lexical tokens from:
-- Tiger.Lexical.Tokens
}
