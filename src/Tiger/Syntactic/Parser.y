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
--     and happyTokenParseWithPosn :: [L Token] -> Program
module Tiger.Syntactic.Parser (happyTokenParse, happyTokenParseWithPosn) where 

import Control.Monad.Except

import Tiger.Lexical.Tokens (Token(..), L(..), AlexPosn(..), getStr, getNum)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Field(..), FunDecl(..), Symbol)
}


%name happyTokenParseWithPosn
%tokentype { L Token }
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
      EOF             { L { getPos=_, unPos=TEOF } }

      ARRAY           { L { getPos=_, unPos=ARRAY } }
      BREAK           { L { getPos=_, unPos=BREAK } }
      DO              { L { getPos=_, unPos=DO } }
      ELSE            { L { getPos=_, unPos=ELSE } }
      END             { L { getPos=_, unPos=END } }
      FOR             { L { getPos=_, unPos=FOR } }
      FUNCTION        { L { getPos=_, unPos=FUNCTION } }
      IF              { L { getPos=_, unPos=IF } }
      IN              { L { getPos=_, unPos=IN } }
      LET             { L { getPos=_, unPos=LET } }
      NIL             { L { getPos=_, unPos=NIL } }
      OF              { L { getPos=_, unPos=OF } }
      TO              { L { getPos=_, unPos=TO } }
      VAR             { L { getPos=_, unPos=VAR } }
      THEN            { L { getPos=_, unPos=THEN } }
      TYPE            { L { getPos=_, unPos=TYPE } }
      WHILE           { L { getPos=_, unPos=WHILE } }
  
      '+'             { L { getPos=_, unPos=PLUS } }
      '-'             { L { getPos=_, unPos=MINUS } }
      '*'             { L { getPos=_, unPos=STAR } }
      '/'             { L { getPos=_, unPos=SLASH } }
      '&'             { L { getPos=_, unPos=AMPERSAND } }
      '|'             { L { getPos=_, unPos=PIPE } }
  
      ':='            { L { getPos=_, unPos=DEFINE } }

      '='             { L { getPos=_, unPos=EQUAL } }
      '<>'            { L { getPos=_, unPos=NOTEQUAL } }
      '>='            { L { getPos=_, unPos=GREATEROREQUAL } }
      '<='            { L { getPos=_, unPos=LESSOREQUAL } }
      '>'             { L { getPos=_, unPos=GREATERTHAN } }
      '<'             { L { getPos=_, unPos=LESSTHAN } }
  
      ';'             { L { getPos=_, unPos=SEMICOLON } }
      ':'             { L { getPos=_, unPos=COLON } }
      '.'             { L { getPos=_, unPos=PERIOD } }
      ','             { L { getPos=_, unPos=COMMA } }
      '{'             { L { getPos=_, unPos=LBRACE } }
      '}'             { L { getPos=_, unPos=RBRACE } }
      '('             { L { getPos=_, unPos=LPAREN } }
      ')'             { L { getPos=_, unPos=RPAREN } }
      '['             { L { getPos=_, unPos=LBRACKET } }
      ']'             { L { getPos=_, unPos=RBRACKET } }
      
      NUM             { L { getPos=_, unPos=NUM n } }
      ID              { L { getPos=_, unPos=ID s } }
      STR             { L { getPos=_, unPos=STR s } }

%%

program     : exp EOF                                               { Program $1 }

exp         : NIL                                                   { NilExp }
            | lvalue                                                { VarExp $1 }
            | NUM                                                   { IntExp (getNum $ unPos $1) }
            | STR                                                   { StrExp (getStr $ unPos $1) (getPos $1) }
            | '(' expseq ')'                                        { if length($2) == 1 then head $2 else SeqExp (reverse $2) }
            | '-' exp %prec NEG                                     { OpExp { left=(IntExp 0), oper=Sub, right=$2, opposn=(getPos $1) } }
            | ID '(' arglist ')'                                    { CallExp { func=((getStr $ unPos $1) :: Symbol), args=(reverse $3), callposn=(getPos $2) } }
            | exp '*' exp                                           { OpExp { left=$1, oper=Mul, right=$3, opposn=(getPos $2) } }
            | exp '/' exp                                           { OpExp { left=$1, oper=Div, right=$3, opposn=(getPos $2) } }
            | exp '+' exp                                           { OpExp { left=$1, oper=Add, right=$3, opposn=(getPos $2) } }
            | exp '-' exp                                           { OpExp { left=$1, oper=Sub, right=$3, opposn=(getPos $2) } }
            | exp '=' exp                                           { OpExp { left=$1, oper=Equal, right=$3, opposn=(getPos $2) } }
            | exp '<>' exp                                          { OpExp { left=$1, oper=NotEqual, right=$3, opposn=(getPos $2) } }
            | exp '>' exp                                           { OpExp { left=$1, oper=GreaterThan, right=$3, opposn=(getPos $2) } }
            | exp '<' exp                                           { OpExp { left=$1, oper=LessThan, right=$3, opposn=(getPos $2) } }
            | exp '>=' exp                                          { OpExp { left=$1, oper=GreaterEqual, right=$3, opposn=(getPos $2) } }
            | exp '<=' exp                                          { OpExp { left=$1, oper=LessEqual, right=$3, opposn=(getPos $2) } }
            | exp '&' exp                                           { IfExp { iftest=$1, thenexp=$3, elseexp=(Just (IntExp 0)), ifposn=(getPos $2) } }
            | exp '|' exp                                           { IfExp { iftest=$1, thenexp=(IntExp 1), elseexp=(Just $3), ifposn=(getPos $2) } }
            | ID '{' recflist '}'                                   { RecordExp { fields=$3, rtyp=((getStr $ unPos $1) :: Symbol), rposn=(getPos $1) } }
            | lvalue ':=' exp                                       { AssignExp { avar=$1, aexp=$3, aPosn=(getPos $2) } }
            | IF exp THEN exp ELSE exp                              { IfExp { iftest=$2, thenexp=$4, elseexp=(Just $6), ifposn=(getPos $1) } }
            | IF exp THEN exp                                       { IfExp { iftest=$2, thenexp=$4, elseexp=Nothing, ifposn=(getPos $1) } }
            | WHILE exp DO exp                                      { WhileExp { wtest=$2, wbody=$4, wPosn=(getPos $1) } }
            | FOR ID ':=' exp TO exp DO exp                         { ForExp { fvar=((getStr $ unPos $2) :: Symbol), fescape=True, lo=$4, hi=$6, fbody=$8, fposn=(getPos $1) } }
            | BREAK                                                 { BreakExp (getPos $1) }
            | LET decllist IN expseq END                            { LetExp { decls=(reverse $2), lbody=(if length($4) == 1 then head $4 else SeqExp (reverse $4)), lposn=(getPos $1) } }
            | ID '[' exp ']' OF exp                                 { ArrayExp { atyp=((getStr $ unPos $1) :: Symbol), size=$3, ainit=$6, aposn=(getPos $1) } }
            
expseq      : {- empty -}                                           { [] }
            | expseq ';' exp                                        { $3 : $1 } 
            | exp                                                   { [$1] }
            
arglist     : {- empty -}                                           { [] }
            | arglist ',' exp                                       { $3 : $1 } 
            | exp                                                   { [$1] }
            
lvalue      : ID                                                    { SimpleVar ((getStr $ unPos $1) :: Symbol) (getPos $1) }
            | lvalue '.' ID                                         { FieldVar $1 ((getStr $ unPos $3) :: Symbol) (getPos $2) }
            | ID '[' exp ']'                                        { SubscriptVar (SimpleVar ((getStr $ unPos $1) :: Symbol) (getPos $1)) $3 (getPos $2) }
            | lvalue '[' exp ']'                                    { SubscriptVar $1 $3 (getPos $2) }
      
decllist    : {- empty -}                                           { [] }
            | decllist decl                                         { $2 : $1 } 
         
decl        : TYPE ID '=' ty                                        { TypeDecl { tname=((getStr $ unPos $2) :: Symbol), ttyp=$4, tposn=(getPos $1) } }
            | vardecl                                               { $1 }
            | fundecllist                                           { FunDecls (reverse $1) }

ty          : ID                                                    { NameType ((getStr $ unPos $1) :: Symbol) (getPos $1) }
            | '{' '}'                                               { RecordType [] }
            | '{' fieldlist '}'                                     { RecordType $2 }
            | ARRAY OF ID                                           { ArrayType ((getStr $ unPos $3) :: Symbol) (getPos $1) }
            
recflist    : recfield                                              { [$1] }
            | recflist ',' recfield                                 { $3 : $1 }
            | {- empty -}                                           { [] }
            
recfield    : ID '=' exp                                            { ((getStr $ unPos $1) :: Symbol, $3) }
   
fieldlist   : field                                                 { [$1] }
            | field ',' fieldlist                                   { $1 : $3 }
            
field       : ID ':' ID                                             { Field { fieldname=((getStr $ unPos $1) :: Symbol), escape=True, typ=((getStr $ unPos $3) :: Symbol), fieldposn=(getPos $1) } }
         
vardecl     : VAR ID ':=' exp                                       { VarDecl { vname=((getStr $ unPos $2) :: Symbol), vescape=True, vtyp=Nothing, vinit=$4, vposn=(getPos $1) } }
            | VAR ID ':' ID ':=' exp                                { VarDecl { vname=((getStr $ unPos $2) :: Symbol), vescape=True, vtyp=(Just ((getStr $ unPos $4) :: Symbol)), vinit=$6, vposn=(getPos $1) } }
            
fundecllist : {- empty -}                                           { [] }
            | fundecllist fundecl                                   { $2 : $1 } 
        
fundecl     : FUNCTION ID '(' ')' '=' exp                           { FunDecl { fundeclname=((getStr $ unPos $2) :: Symbol), params=[], result=(Nothing, getPos $5), body=$6, funposn=(getPos $1) } }
            | FUNCTION ID '(' fieldlist ')' '=' exp                 { FunDecl { fundeclname=((getStr $ unPos $2) :: Symbol), params=$4, result=(Nothing, getPos $6), body=$7, funposn=(getPos $1) } }
            | FUNCTION ID '(' ')' ':' ID '=' exp                    { FunDecl { fundeclname=((getStr $ unPos $2) :: Symbol), params=[], result=((Just ((getStr $ unPos $6) :: Symbol)), getPos $5), body=$8, funposn=(getPos $1) } }
            | FUNCTION ID '(' fieldlist ')' ':' ID '=' exp          { FunDecl { fundeclname=((getStr $ unPos $2) :: Symbol), params=$4, result=((Just ((getStr $ unPos $7) :: Symbol)), getPos $6), body=$9, funposn=(getPos $1) } }
     
{

-- |Convenience function used in testing, uses fake metadata
happyTokenParse :: [Token] -> Program
happyTokenParse ts = happyTokenParseWithPosn $ map (\t -> L {getPos=AlexPosn {absolute=1,row=1,col=1}, unPos=t}) ts

parseError :: [L Token] -> a
parseError tokens = error ("Parse error: (Line " ++ show errRow ++ ", Col " ++ show errCol ++ "): " ++ show nextTok)
    where errRow = row $ getPos next
          errCol = col $ getPos next
          nextTok = unPos next
          next = head tokens

-- Could include types representing syntax tree nodes,
-- but we already imported that above from:
-- Tiger.Syntactic.Syntax

-- We also already included the lexical tokens from:
-- Tiger.Lexical.Tokens

}
