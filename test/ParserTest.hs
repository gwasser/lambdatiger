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
import Tiger.Syntactic.Parser (happyTokenParse, happyTokenParseWithPosn)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))


-- these tests are NOT necessarily valid Tiger programs, but only syntactically
-- correct constructs to exercise the parser
parserTests = testGroup "Happy-based Tiger parser" [testParserNil, testParserIfNil, testParserIfThenElse, testParserIfThenIfThenElse, testParserWhileDo, testParserProperForLoop, testParserSubscriptVar, testParserSeq, testParserArray, testParserLetVarDecl, testParserLetVarDecl2, testParserLetVarDecl3,testParserLetRecursiveFuncs, testParserTypeDecls, testParserLetFuncDecl]

testParserNil =
  testCase "parses 'NIL'" $ assertEqual [] (Program NilExp) (happyTokenParse $ alexMonadScanTokens "nil")
testParserIfNil =
  testCase "does not parse 'IF NIL'" $ assertEqual [] (Program NilExp) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "if nil")
testParserIfThenElse =
    testCase "parses 'if x then y else z'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x" (AlexPosn { row=1, col=4, absolute=4 }))) (VarExp (SimpleVar "y" (AlexPosn { row=1, col=11, absolute=11 }))) (Just (VarExp (SimpleVar "z" (AlexPosn { row=1, col=18, absolute=18 })))) (AlexPosn { row=1, col=1, absolute=1 })) ) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "if x then y else z")
testParserIfThenIfThenElse =
  testCase "parses 'if x then if a then b else c'" $ assertEqual [] (Program (IfExp {iftest = VarExp (SimpleVar "x" (AlexPosn {absolute = 4, row = 1, col = 4})), thenexp = IfExp {iftest = VarExp (SimpleVar "a" (AlexPosn {absolute = 14, row = 1, col = 14})), thenexp = VarExp (SimpleVar "b" (AlexPosn {absolute = 21, row = 1, col = 21})), elseexp = Just (VarExp (SimpleVar "c" (AlexPosn {absolute = 28, row = 1, col = 28}))), ifposn = AlexPosn {absolute = 11, row = 1, col = 11}}, elseexp = Nothing, ifposn = AlexPosn {absolute = 1, row = 1, col = 1}}) ) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "if x then if a then b else c")
testParserWhileDo =
  testCase "parses 'while isTrue do 1234'" $ assertEqual [] (Program (WhileExp (VarExp (SimpleVar "isTrue" (AlexPosn {absolute = 7, row = 1, col = 7}))) (IntExp 1234) (AlexPosn {absolute = 1, row = 1, col = 1}))) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "while isTrue do 1234")
testParserProperForLoop =
  testCase "parses 'for t0 := 0 to 10 do someFunc()'" $ assertEqual [] (Program $ ForExp "t0" True (IntExp 0) (IntExp 10) (CallExp "someFunc" [] (AlexPosn {absolute = 30, row = 1, col = 30})) (AlexPosn {absolute = 1, row = 1, col = 1}) ) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "for t0 := 0 to 10 do someFunc()")
testParserSubscriptVar =
  testCase "parses 'list[0]'" $ assertEqual [] (Program $ VarExp $ SubscriptVar (SimpleVar "list" (AlexPosn {absolute = 1, row = 1, col = 1})) (IntExp 0) (AlexPosn {absolute = 5, row = 1, col = 5})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "list[0]")
-- from Appel page 97
testParserSeq =
  testCase "parses '(a := 5; a+1)'" $ assertEqual [] (Program $ SeqExp [AssignExp (SimpleVar "a" (AlexPosn {absolute = 2, row = 1, col = 2})) (IntExp 5) (AlexPosn {absolute = 4, row = 1, col = 4}), OpExp (VarExp $ SimpleVar "a" (AlexPosn {absolute = 10, row = 1, col = 10})) (Add) (IntExp 1) (AlexPosn {absolute = 11, row = 1, col = 11})]) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "(a := 5; a+1)")
testParserArray =
  testCase "parses 'int[3+7] of 5'" $ assertEqual [] (Program $ ArrayExp "int" (OpExp (IntExp 3) Add (IntExp 7) (AlexPosn {absolute = 6, row = 1, col = 6})) (IntExp 5) (AlexPosn {absolute = 1, row = 1, col = 1})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "int[3+7] of 5")
testParserLetVarDecl =
  testCase "parses 'let var diag2 := intArray [N+N-1] of 0 in try(0) end'" $ assertEqual [] (Program (LetExp {decls = [VarDecl {vname = "diag2", vescape = True, vtyp = Nothing, vinit = ArrayExp {atyp = "intArray", size = OpExp {left = OpExp {left = VarExp (SimpleVar "N" (AlexPosn {absolute = 28, row = 1, col = 28})), oper = Add, right = VarExp (SimpleVar "N" (AlexPosn {absolute = 30, row = 1, col = 30})), opposn = AlexPosn {absolute = 29, row = 1, col = 29}}, oper = Sub, right = IntExp 1, opposn = AlexPosn {absolute = 31, row = 1, col = 31}}, ainit = IntExp 0, aposn = AlexPosn {absolute = 18, row = 1, col = 18}}, vposn = AlexPosn {absolute = 5, row = 1, col = 5}}], lbody = CallExp {func = "try", args = [IntExp 0], callposn = AlexPosn {absolute = 46, row = 1, col = 46}}, lposn = AlexPosn {absolute = 1, row = 1, col = 1}})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "let var diag2 := intArray [N+N-1] of 0 in try(0) end")
testParserLetVarDecl2 =
  testCase "parses 'let type intArray = array of int in try(0, 1) end'" $ assertEqual [] (Program (LetExp {decls = [TypeDecl {tname = "intArray", ttyp = ArrayType "int" (AlexPosn {absolute = 21, row = 1, col = 21}), tposn = AlexPosn {absolute = 5, row = 1, col = 5}}], lbody = CallExp {func = "try", args = [IntExp 0,IntExp 1], callposn = AlexPosn {absolute = 40, row = 1, col = 40}}, lposn = AlexPosn {absolute = 1, row = 1, col = 1}})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "let type intArray = array of int in try(0, 1) end")
testParserLetVarDecl3 =
    testCase "parses 'let type any = { any : int } var buffer := getchar()...'" $ assertEqual [] (Program (LetExp {decls = [TypeDecl {tname = "any", ttyp = RecordType [Field {fieldname = "any", escape = True, typ = "int", fieldposn = AlexPosn {absolute = 18, row = 1, col = 18}}], tposn = AlexPosn {absolute = 5, row = 1, col = 5}},VarDecl {vname = "buffer", vescape = True, vtyp = Nothing, vinit = CallExp {func = "getchar", args = [], callposn = AlexPosn {absolute = 51, row = 1, col = 51}}, vposn = AlexPosn {absolute = 30, row = 1, col = 30}}], lbody = CallExp {func = "try", args = [IntExp 0,IntExp 1], callposn = AlexPosn {absolute = 60, row = 1, col = 60}}, lposn = AlexPosn {absolute = 1, row = 1, col = 1}})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "let type any = { any : int } var buffer := getchar() in try(0, 1) end")
-- from Appel page 97, soln on 99
testParserLetRecursiveFuncs =
  testCase "parses 'let...' recursive funcs" $ assertEqual [] (Program (LetExp {decls = [VarDecl {vname = "a", vescape = True, vtyp = Nothing, vinit = IntExp 5, vposn = AlexPosn {absolute = 5, row = 1, col = 5}},FunDecls [FunDecl {fundeclname = "f", params = [], result = (Just "int",AlexPosn {absolute = 29, row = 1, col = 29}), body = CallExp {func = "g", args = [VarExp (SimpleVar "a" (AlexPosn {absolute = 39, row = 1, col = 39}))], callposn = AlexPosn {absolute = 38, row = 1, col = 38}}, funposn = AlexPosn {absolute = 16, row = 1, col = 16}},FunDecl {fundeclname = "g", params = [Field {fieldname = "i", escape = True, typ = "int", fieldposn = AlexPosn {absolute = 53, row = 1, col = 53}}], result = (Nothing,AlexPosn {absolute = 62, row = 1, col = 62}), body = CallExp {func = "f", args = [], callposn = AlexPosn {absolute = 65, row = 1, col = 65}}, funposn = AlexPosn {absolute = 42, row = 1, col = 42}}]], lbody = CallExp {func = "f", args = [], callposn = AlexPosn {absolute = 72, row = 1, col = 72}}, lposn = AlexPosn {absolute = 1, row = 1, col = 1}})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "let var a := 5 function f() : int = g(a) function g(i : int) = f() in f() end")
-- from Appel page 99
testParserTypeDecls =
    testCase "parses two type decls" $ assertEqual [] (Program (LetExp {decls = [TypeDecl {tname = "tree", ttyp = RecordType [Field {fieldname = "key", escape = True, typ = "int", fieldposn = AlexPosn {absolute = 18, row = 1, col = 18}},Field {fieldname = "children", escape = True, typ = "treelist", fieldposn = AlexPosn {absolute = 28, row = 1, col = 28}}], tposn = AlexPosn {absolute = 5, row = 1, col = 5}},TypeDecl {tname = "treelist", ttyp = RecordType [Field {fieldname = "head", escape = True, typ = "tree", fieldposn = AlexPosn {absolute = 65, row = 1, col = 65}},Field {fieldname = "tail", escape = True, typ = "treelist", fieldposn = AlexPosn {absolute = 77, row = 1, col = 77}}], tposn = AlexPosn {absolute = 48, row = 1, col = 48}}], lbody = IntExp 0, lposn = AlexPosn {absolute = 1, row = 1, col = 1}})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "let type tree = {key: int, children: treelist} type treelist = {head: tree, tail: treelist} in 0 end")
testParserLetFuncDecl =
  testCase "parses 'let function printboard()...'" $ assertEqual [] (Program (LetExp {decls = [FunDecls [FunDecl {fundeclname = "printboard", params = [], result = (Nothing,AlexPosn {absolute = 27, row = 1, col = 27}), body = SeqExp [ForExp {fvar = "i", fescape = True, lo = IntExp 0, hi = OpExp {left = VarExp (SimpleVar "N" (AlexPosn {absolute = 44, row = 1, col = 44})), oper = Sub, right = IntExp 1, opposn = AlexPosn {absolute = 45, row = 1, col = 45}}, fbody = SeqExp [ForExp {fvar = "j", fescape = True, lo = IntExp 0, hi = OpExp {left = VarExp (SimpleVar "N" (AlexPosn {absolute = 66, row = 1, col = 66})), oper = Sub, right = IntExp 1, opposn = AlexPosn {absolute = 67, row = 1, col = 67}}, fbody = CallExp {func = "print", args = [IfExp {iftest = OpExp {left = VarExp (SubscriptVar (SimpleVar "col" (AlexPosn {absolute = 82, row = 1, col = 82})) (VarExp (SimpleVar "i" (AlexPosn {absolute = 86, row = 1, col = 86}))) (AlexPosn {absolute = 85, row = 1, col = 85})), oper = Equal, right = VarExp (SimpleVar "j" (AlexPosn {absolute = 89, row = 1, col = 89})), opposn = AlexPosn {absolute = 88, row = 1, col = 88}}, thenexp = StrExp " O" (AlexPosn {absolute = 99, row = 1, col = 99}), elseexp = Just (StrExp " ." (AlexPosn {absolute = 109, row = 1, col = 109})), ifposn = AlexPosn {absolute = 79, row = 1, col = 79}}], callposn = AlexPosn {absolute = 78, row = 1, col = 78}}, fposn = AlexPosn {absolute = 52, row = 1, col = 52}},CallExp {func = "print", args = [StrExp "\n" (AlexPosn {absolute = 121, row = 1, col = 121})], callposn = AlexPosn {absolute = 118, row = 1, col = 118}}], fposn = AlexPosn {absolute = 30, row = 1, col = 30}},CallExp {func = "print", args = [StrExp "\n" (AlexPosn {absolute = 134, row = 2, col = 13})], callposn = AlexPosn {absolute = 131, row = 2, col = 10}}], funposn = AlexPosn {absolute = 5, row = 1, col = 5}}]], lbody = CallExp {func = "try", args = [IntExp 0], callposn = AlexPosn {absolute = 144, row = 3, col = 10}}, lposn = AlexPosn {absolute = 1, row = 1, col = 1}})) (happyTokenParseWithPosn $ alexMonadScanTokensWithPosn "let function printboard() = (for i := 0 to N-1 do (for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end")

                                                                                                                                                                                                                                                        
                             
