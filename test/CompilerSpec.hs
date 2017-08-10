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

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tiger.Lexical.Lexer (scanner)
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntactic.Parser (happyTokenParse)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Unit Tests" [tokenizerTests, parserTests, appelTests]

-- these tests are NOT necessarily valid Tiger programs, but only lists
-- of tokens to check that the lexer does what I expect
tokenizerTests = testGroup "Alex-based Tiger lexer" [testLexerARRAY, testLexerArraysID, testLexerArraID, testLexerNIL, testLexerIfThenElse, testLexerIfThenIfThenElse, testLexerWhileDo, testLexerIgnoreComments, testLexerProperForLoop, testLexerBoolArith, testLexerParens, testLexerNestedComments, testLexerMultComments, testLexerNoStringInComments, testLexerNoCommentInStrings, testStringLiterals, testStringLiteralsWithEscapes, testMultilineString, testLexerSubscriptVar, testLexerLetFuncDec]

testLexerARRAY =
  testCase "accepts input 'array' as ARRAY" $ assertEqual [] ([ARRAY, TEOF]) (scanner "array")
testLexerArraysID =
  testCase "accepts input 'arrays' as (ID 'arrays')" $ assertEqual [] ([ID "arrays", TEOF]) (scanner "arrays")
testLexerArraID =
  testCase "accepts input 'arra' as (ID 'arra')" $ assertEqual [] ([ID "arra", TEOF]) (scanner "arra")
testLexerNIL =
  testCase "accepts input 'nil' as NIL" $ assertEqual [] ([NIL, TEOF]) (scanner "nil")
testLexerIfThenElse =
  testCase "accepts input 'if x then y else z'" $ assertEqual [] ([IF, ID "x", THEN, ID "y", ELSE, ID "z", TEOF]) (scanner "if x then y else z")
testLexerIfThenIfThenElse =
  testCase "accepts input 'if x then if a then b else c'" $ assertEqual [] ([IF, ID "x", THEN, IF, ID "a", THEN, ID "b", ELSE, ID "c", TEOF]) (scanner "if x then if a then b else c")
testLexerWhileDo =
  testCase "accepts input 'while isTrue do 1234'" $ assertEqual [] ([WHILE, ID "isTrue", DO, NUM 1234, TEOF]) (scanner "while isTrue do 1234")
testLexerIgnoreComments =
  testCase "accepts input 'for t0 /* some comment */ in someList'" $ assertEqual [] ([FOR, ID "t0", IN, ID "someList", TEOF]) (scanner "for t0 /* some comment */ in someList")
testLexerProperForLoop =
  testCase "accepts input 'for t0 := 0 to 10 do someFunc()'" $ assertEqual [] ([FOR, ID "t0", DEFINE, NUM 0, TO, NUM 10, DO, ID "someFunc", LPAREN, RPAREN, TEOF]) (scanner "for t0 := 0 to 10 do someFunc()")
testLexerArith =
  testCase "accepts input 'x = y + 42'" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42, TEOF]) (scanner "x = y + 42")
testLexerBoolArith =
  testCase "accepts input 'b := a | b & c;'" $ assertEqual [] ([ID "b", DEFINE, ID "a", PIPE, ID "b", AMPERSAND, ID "c", SEMICOLON, TEOF]) (scanner "b := a | b & c;")
testLexerParens =
  testCase "accepts input '12* (42 - 17)/{5}   +a[1]'" $ assertEqual [] ([NUM 12, STAR, LPAREN, NUM 42, MINUS, NUM 17, RPAREN, SLASH, LBRACE, NUM 5, RBRACE, PLUS, ID "a", LBRACKET, NUM 1, RBRACKET, TEOF]) (scanner "12* (42 - 17)/{5}   +a[1]")
testLexerNestedComments =
  testCase "accepts input 'x = y /* outer /* inner comment */ outer */ + 42'" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42, TEOF]) (scanner "x = y /* outer /* inner comment */ outer */ + 42")
testLexerMultComments =
  testCase "accepts input '/* a */ x = y /* b? */ + /* answer to everything */ 42'" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42, TEOF]) (scanner "/* a */ x = y /* b? */ + /* answer to everything */ 42")
testLexerNoStringInComments =
  testCase "accepts input 'x = /* here is \" a string that does not count \" */ 42'" $ assertEqual [] ([ID "x", EQUAL, NUM 42, TEOF]) (scanner "x = /* here is \" a string that does not count \" */ 42")
testLexerNoCommentInStrings =
  testCase "accepts input 'x = \" should not /* see a comment */ \"'" $ assertEqual [] ([ID "x", EQUAL, STR " should not /* see a comment */ ", TEOF]) (scanner "x = \" should not /* see a comment */ \"")
testStringLiterals =
    testCase "accepts input 'if \"string\" = \"other\" then x'" $ assertEqual [] ([IF, STR "string", EQUAL, STR "other", THEN, ID "x", TEOF]) (scanner "if \"string\" = \"other\" then x")
testStringLiteralsWithEscapes =
    testCase "accepts input 'if \"abc\n\" = \"lmn\t\" then x'" $ assertEqual [] ([IF, STR "abc\n", EQUAL, STR "lmn\t", THEN, ID "x", TEOF]) (scanner "if \"abc\n\" = \"lmn\t\" then x")
testMultilineString =
    testCase "accepts multiline string as input" $ assertEqual [] ([IF, ID "x", THEN, ID "y", ELSE, ID "z", TEOF]) (scanner "if x \
                                    \then y \
                                    \else z")
testLexerSubscriptVar =
  testCase "accepts input 'list[0]'" $ assertEqual [] ([ID "list", LBRACKET, NUM 0, RBRACKET, TEOF]) (scanner "list[0]")
testLexerLetFuncDec =
    testCase "accepts input 'let function printboard()...'" $ assertEqual [] ([LET, FUNCTION, ID "printboard", LPAREN, RPAREN, EQUAL, LPAREN, FOR, ID "i", DEFINE, NUM 0, TO, ID "N", MINUS, NUM 1, DO, LPAREN, FOR, ID "j", DEFINE, NUM 0, TO, ID "N", MINUS, NUM 1, DO, ID "print", LPAREN, IF, ID "col", LBRACKET, ID "i", RBRACKET, EQUAL, ID "j", THEN, STR " O", ELSE, STR " .", RPAREN, SEMICOLON, ID "print", LPAREN, STR "\n", RPAREN, RPAREN, SEMICOLON, ID "print", LPAREN, STR "\n", RPAREN, RPAREN, IN, ID "try", LPAREN, NUM 0, RPAREN, END, TEOF]) (scanner "let function printboard() = (for i := 0 to N-1 do (for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end")   


-- these tests are NOT necessarily valid Tiger programs, but only syntactically
-- correct constructs to exercise the parser
parserTests = testGroup "Happy-based Tiger parser" [testParserNil, testParserIfThenElse, testParserIfThenIfThenElse, testParserWhileDo, testParserProperForLoop, testParserSubscriptVar, testParserSeq, testParserArray, testParserLetVarDecl, testParserLetVarDecl2, testParserLetVarDecl3,testParserLetRecursiveFuncs, testParserTypeDecls, testParserLetFuncDecl]

testParserNil =
  testCase "parses 'NIL'" $ assertEqual [] (Program NilExp) (happyTokenParse $ scanner "nil")
testParserIfThenElse =
  testCase "parses 'if x then y else z'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x")) (VarExp (SimpleVar "y")) (Just (VarExp (SimpleVar "z")))) ) (happyTokenParse $ scanner "if x then y else z")
testParserIfThenIfThenElse =
  testCase "parses 'if x then if a then b else c'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x")) (IfExp (VarExp (SimpleVar "a")) (VarExp (SimpleVar "b")) (Just (VarExp (SimpleVar "c")))) (Nothing) ) ) (happyTokenParse $ scanner "if x then if a then b else c")
testParserWhileDo =
  testCase "parses 'while isTrue do 1234'" $ assertEqual [] (Program (WhileExp (VarExp (SimpleVar "isTrue")) (IntExp 1234))) (happyTokenParse $ scanner "while isTrue do 1234")
testParserProperForLoop =
  testCase "parses 'for t0 := 0 to 10 do someFunc()'" $ assertEqual [] (Program $ ForExp "t0" True (IntExp 0) (IntExp 10) (CallExp "someFunc" []) ) (happyTokenParse $ scanner "for t0 := 0 to 10 do someFunc()")
testParserSubscriptVar =
  testCase "parses 'list[0]'" $ assertEqual [] (Program $ VarExp $ SubscriptVar (SimpleVar "list") (IntExp 0)) (happyTokenParse $ scanner "list[0]")
-- from Appel page 97
testParserSeq =
  testCase "parses '(a := 5; a+1)'" $ assertEqual [] (Program $ SeqExp [AssignExp (SimpleVar "a") (IntExp 5), OpExp (VarExp $ SimpleVar "a") (Add) (IntExp 1)]) (happyTokenParse $ scanner "(a := 5; a+1)")
testParserArray =
  testCase "parses 'int[3+7] of 5'" $ assertEqual [] (Program $ ArrayExp "int" (OpExp (IntExp 3) Add (IntExp 7)) (IntExp 5)) (happyTokenParse $ scanner "int[3+7] of 5")
testParserLetVarDecl =
  testCase "parses 'let var diag2 := intArray [N+N-1] of 0 in try(0) end'" $ assertEqual [] (Program $ LetExp [VarDecl "diag2" True Nothing (ArrayExp "intArray" (OpExp (OpExp (VarExp (SimpleVar "N")) Add (VarExp (SimpleVar "N"))) Sub (IntExp 1)) (IntExp 0))] (CallExp "try" [IntExp 0])) (happyTokenParse $ scanner "let var diag2 := intArray [N+N-1] of 0 in try(0) end")
testParserLetVarDecl2 =
  testCase "parses 'let type intArray = array of int in try(0, 1) end'" $ assertEqual [] (Program $ LetExp [TypeDecl "intArray" (ArrayType "int")] (CallExp "try" [IntExp 0, IntExp 1])) (happyTokenParse $ scanner "let type intArray = array of int in try(0, 1) end")
testParserLetVarDecl3 =
    testCase "parses 'let type any = { any : int } var buffer := getchar()...'" $ assertEqual [] (Program $ LetExp [TypeDecl "any" (RecordType [Field "any" True "int"]), VarDecl "buffer" True Nothing (CallExp "getchar" [])] (CallExp "try" [IntExp 0, IntExp 1])) (happyTokenParse $ scanner "let type any = { any : int } var buffer := getchar() in try(0, 1) end")
-- from Appel page 97, soln on 99
testParserLetRecursiveFuncs =
  testCase "parses 'let...' recursive funcs" $ assertEqual [] (Program $ LetExp [VarDecl "a" True Nothing (IntExp 5), FunDecls [FunDecl "f" [] (Just "int") (CallExp "g" [VarExp $ SimpleVar "a"]), FunDecl "g" [Field "i" True "int"] Nothing (CallExp "f" [])]] (CallExp "f" [])) (happyTokenParse $ scanner "let var a := 5 function f() : int = g(a) function g(i : int) = f() in f() end")
-- from Appel page 99
testParserTypeDecls =
    testCase "parses two type decls" $ assertEqual [] (Program $ LetExp [TypeDecl "tree" (RecordType [Field "key" True "int", Field "children" True "treelist"]), TypeDecl "treelist" (RecordType [Field "head" True "tree", Field "tail" True "treelist"])] (IntExp 0)) (happyTokenParse $ scanner "let type tree = {key: int, children: treelist} type treelist = {head: tree, tail: treelist} in 0 end")
testParserLetFuncDecl =
  testCase "parses 'let function printboard()...'" $ assertEqual [] (Program $ LetExp [FunDecls [FunDecl "printboard" [] Nothing (SeqExp [ForExp "i" True (IntExp 0) (OpExp (VarExp $ SimpleVar "N") Sub (IntExp 1)) (     (SeqExp [ForExp "j" True (IntExp 0) (OpExp (VarExp $ SimpleVar "N") Sub (IntExp 1)) (CallExp "print" [IfExp (OpExp (VarExp $ SubscriptVar (SimpleVar "col") (VarExp $ SimpleVar "i")) Equal (VarExp $ SimpleVar "j")) (StrExp " O") (Just $ StrExp " .")]), (CallExp "print" [StrExp "\n"])])    ), (CallExp "print" [StrExp "\n"])])]] (CallExp "try" [IntExp 0])) (happyTokenParse $ scanner "let function printboard() = (for i := 0 to N-1 do (for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end")
                                                                                                                                                                                                                                                        
                             
                             
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
                                                                                                                                                                                                                                                        
