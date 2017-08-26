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

module ParserTest where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tiger.Lexical.Lexer (alexMonadScanTokens, alexMonadScanTokensWithPosn)
import Tiger.Lexical.Tokens (Token(..), L(..), AlexPosn(..))
import Tiger.Syntactic.Parser (happyTokenParse)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))


-- these tests are NOT necessarily valid Tiger programs, but only syntactically
-- correct constructs to exercise the parser
parserTests = testGroup "Happy-based Tiger parser" [testParserNil, testParserIfThenElse]
    
    --testParserIfThenIfThenElse, testParserWhileDo, testParserProperForLoop, testParserSubscriptVar, testParserSeq, testParserArray, testParserLetVarDecl, testParserLetVarDecl2, testParserLetVarDecl3,testParserLetRecursiveFuncs, testParserTypeDecls, testParserLetFuncDecl]

testParserNil =
  testCase "parses 'NIL'" $ assertEqual [] (Program NilExp) (happyTokenParse $ alexMonadScanTokens "nil")
testParserIfThenElse =
    testCase "parses 'if x then y else z'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x" (AlexPosn { row=1, col=4, absolute=4 }))) (VarExp (SimpleVar "y" (AlexPosn { row=1, col=11, absolute=11 }))) (Just (VarExp (SimpleVar "z" (AlexPosn { row=1, col=18, absolute=18 })))) (AlexPosn { row=1, col=1, absolute=1 })) ) (happyTokenParse $ alexMonadScanTokens "if x then y else z")
{-
testParserIfThenIfThenElse =
  testCase "parses 'if x then if a then b else c'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x")) (IfExp (VarExp (SimpleVar "a")) (VarExp (SimpleVar "b")) (Just (VarExp (SimpleVar "c")))) (Nothing) ) ) (happyTokenParse $ alexMonadScanTokens "if x then if a then b else c")
testParserWhileDo =
  testCase "parses 'while isTrue do 1234'" $ assertEqual [] (Program (WhileExp (VarExp (SimpleVar "isTrue")) (IntExp 1234))) (happyTokenParse $ alexMonadScanTokens "while isTrue do 1234")
testParserProperForLoop =
  testCase "parses 'for t0 := 0 to 10 do someFunc()'" $ assertEqual [] (Program $ ForExp "t0" True (IntExp 0) (IntExp 10) (CallExp "someFunc" []) ) (happyTokenParse $ alexMonadScanTokens "for t0 := 0 to 10 do someFunc()")
testParserSubscriptVar =
  testCase "parses 'list[0]'" $ assertEqual [] (Program $ VarExp $ SubscriptVar (SimpleVar "list") (IntExp 0)) (happyTokenParse $ alexMonadScanTokens "list[0]")
-- from Appel page 97
testParserSeq =
  testCase "parses '(a := 5; a+1)'" $ assertEqual [] (Program $ SeqExp [AssignExp (SimpleVar "a") (IntExp 5), OpExp (VarExp $ SimpleVar "a") (Add) (IntExp 1)]) (happyTokenParse $ alexMonadScanTokens "(a := 5; a+1)")
testParserArray =
  testCase "parses 'int[3+7] of 5'" $ assertEqual [] (Program $ ArrayExp "int" (OpExp (IntExp 3) Add (IntExp 7)) (IntExp 5)) (happyTokenParse $ alexMonadScanTokens "int[3+7] of 5")
testParserLetVarDecl =
  testCase "parses 'let var diag2 := intArray [N+N-1] of 0 in try(0) end'" $ assertEqual [] (Program $ LetExp [VarDecl "diag2" True Nothing (ArrayExp "intArray" (OpExp (OpExp (VarExp (SimpleVar "N")) Add (VarExp (SimpleVar "N"))) Sub (IntExp 1)) (IntExp 0))] (CallExp "try" [IntExp 0])) (happyTokenParse $ alexMonadScanTokens "let var diag2 := intArray [N+N-1] of 0 in try(0) end")
testParserLetVarDecl2 =
  testCase "parses 'let type intArray = array of int in try(0, 1) end'" $ assertEqual [] (Program $ LetExp [TypeDecl "intArray" (ArrayType "int")] (CallExp "try" [IntExp 0, IntExp 1])) (happyTokenParse $ alexMonadScanTokens "let type intArray = array of int in try(0, 1) end")
testParserLetVarDecl3 =
    testCase "parses 'let type any = { any : int } var buffer := getchar()...'" $ assertEqual [] (Program $ LetExp [TypeDecl "any" (RecordType [Field "any" True "int"]), VarDecl "buffer" True Nothing (CallExp "getchar" [])] (CallExp "try" [IntExp 0, IntExp 1])) (happyTokenParse $ alexMonadScanTokens "let type any = { any : int } var buffer := getchar() in try(0, 1) end")
-- from Appel page 97, soln on 99
testParserLetRecursiveFuncs =
  testCase "parses 'let...' recursive funcs" $ assertEqual [] (Program $ LetExp [VarDecl "a" True Nothing (IntExp 5), FunDecls [FunDecl "f" [] (Just "int") (CallExp "g" [VarExp $ SimpleVar "a"]), FunDecl "g" [Field "i" True "int"] Nothing (CallExp "f" [])]] (CallExp "f" [])) (happyTokenParse $ alexMonadScanTokens "let var a := 5 function f() : int = g(a) function g(i : int) = f() in f() end")
-- from Appel page 99
testParserTypeDecls =
    testCase "parses two type decls" $ assertEqual [] (Program $ LetExp [TypeDecl "tree" (RecordType [Field "key" True "int", Field "children" True "treelist"]), TypeDecl "treelist" (RecordType [Field "head" True "tree", Field "tail" True "treelist"])] (IntExp 0)) (happyTokenParse $ alexMonadScanTokens "let type tree = {key: int, children: treelist} type treelist = {head: tree, tail: treelist} in 0 end")
testParserLetFuncDecl =
  testCase "parses 'let function printboard()...'" $ assertEqual [] (Program $ LetExp [FunDecls [FunDecl "printboard" [] Nothing (SeqExp [ForExp "i" True (IntExp 0) (OpExp (VarExp $ SimpleVar "N") Sub (IntExp 1)) (     (SeqExp [ForExp "j" True (IntExp 0) (OpExp (VarExp $ SimpleVar "N") Sub (IntExp 1)) (CallExp "print" [IfExp (OpExp (VarExp $ SubscriptVar (SimpleVar "col") (VarExp $ SimpleVar "i")) Equal (VarExp $ SimpleVar "j")) (StrExp " O") (Just $ StrExp " .")]), (CallExp "print" [StrExp "\n"])])    ), (CallExp "print" [StrExp "\n"])])]] (CallExp "try" [IntExp 0])) (happyTokenParse $ alexMonadScanTokens "let function printboard() = (for i := 0 to N-1 do (for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end")
  -}
                                                                                                                                                                                                                                                        
                             
