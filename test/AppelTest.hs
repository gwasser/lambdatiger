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

module AppelTest where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tiger.Lexical.Lexer (scanner)
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntactic.Parser (happyTokenParse)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))
                             
-- these tests are based on the ones written by Appel in sample ML code,
-- and distributed via the textbook website
appelTests = testGroup "Appel's original tests for Tiger" [testAppel1, testAppel2, testAppel3, testAppel4, testAppel5, testAppel12]

testAppel1 =
    testCase "parses test1" $ assertEqual [] (Program $ LetExp [TypeDecl "arrtype" (ArrayType "int"), VarDecl "arr1" True (Just "arrtype") (ArrayExp "arrtype" (IntExp 10) (IntExp 0))] (VarExp $ SimpleVar "arr1")) (happyTokenParse $ scanner "let type arrtype = array of int var arr1:arrtype := arrtype [10] of 0 in arr1 end")
testAppel2 =
    testCase "parses test2" $ assertEqual [] (Program $ LetExp [TypeDecl "myint" (NameType "int"), TypeDecl "arrtype" (ArrayType "myint"), VarDecl "arr1" True (Just "arrtype") (ArrayExp "arrtype" (IntExp 10) (IntExp 0))] (VarExp $ SimpleVar "arr1")) (happyTokenParse $ scanner "let type myint = int type  arrtype = array of myint var arr1:arrtype := arrtype [10] of 0 in arr1 end")
testAppel3 =
    testCase "parses test3" $ assertEqual [] (Program $ LetExp [TypeDecl "rectype" (RecordType [Field "name" True "string", Field "age" True "int"]), VarDecl "rec1" True (Just "rectype") (RecordExp [("age", IntExp 1000), ("name", StrExp "Nobody")] ("rectype"))] (SeqExp [AssignExp (FieldVar (SimpleVar "rec1") "name") (StrExp "Somebody"), VarExp $ SimpleVar "rec1"])) (happyTokenParse $ scanner "let \
	\type  rectype = {name:string, age:int} \
	\var rec1:rectype := rectype {name=\"Nobody\", age=1000} \
\in \
	\rec1.name := \"Somebody\"; \
	\rec1 \
\end")
testAppel4 =
    testCase "parses test4" $ assertEqual [] (Program (LetExp {decls = [FunDecls [FunDecl {fundeclname = "nfactor", params = [Field {fieldname = "n", escape = True, typ = "int"}], result = Just "int", body = IfExp {iftest = OpExp {left = VarExp (SimpleVar "n"), oper = Equal, right = IntExp 0}, thenexp = IntExp 1, elseexp = Just (OpExp {left = VarExp (SimpleVar "n"), oper = Mul, right = CallExp {func = "nfactor", args = [OpExp {left = VarExp (SimpleVar "n"), oper = Sub, right = IntExp 1}]}})}}]], lbody = CallExp {func = "nfactor", args = [IntExp 10]}})) (happyTokenParse $ scanner "let \
\/* calculate n! */ \
\function nfactor(n: int): int = \
	\if  n = 0 \
        \then 1 \
        \else n * nfactor(n-1) \
\in \
	\nfactor(10) \
\end")
testAppel5 =
    testCase "parses test5" $ assertEqual [] (Program (LetExp {decls = [TypeDecl {tname = "intlist", ttyp = RecordType [Field {fieldname = "hd", escape = True, typ = "int"},Field {fieldname = "tl", escape = True, typ = "intlist"}]}, TypeDecl {tname = "tree", ttyp = RecordType [Field {fieldname = "key", escape = True, typ = "int"},Field {fieldname = "children", escape = True, typ = "treelist"}]},TypeDecl {tname = "treelist", ttyp = RecordType [Field {fieldname = "hd", escape = True, typ = "tree"},Field {fieldname = "tl", escape = True, typ = "treelist"}]},VarDecl {vname = "lis", vescape = True, vtyp = Just "intlist", vinit = RecordExp {fields = [("tl",NilExp),("hd",IntExp 0)], rtyp = "intlist"}}], lbody = VarExp (SimpleVar "lis")})) (happyTokenParse $ scanner "let \
\/* define a list */ \
\type intlist = {hd: int, tl: intlist} \ 
\/* define a tree */ \
\type tree ={key: int, children: treelist} \
\type treelist = {hd: tree, tl: treelist} \
\var lis:intlist := intlist { hd=0, tl= nil } \ 
\in \
	\lis \
\end")
testAppel12 =
  testCase "parses test12" $ assertEqual [] (Program $ LetExp [VarDecl "a" True Nothing (IntExp 0)] (ForExp "i" True (IntExp 0) (IntExp 100) (SeqExp [AssignExp (SimpleVar "a") (OpExp (VarExp $ SimpleVar "a") Add (IntExp 1)), SeqExp []]))) (happyTokenParse $ scanner "/* valid for and let */ let var a:= 0 in for i:=0 to 100 do (a:=a+1;()) end")
                                                                                                                                                                                                                                                        
