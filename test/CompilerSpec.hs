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

import Tiger.Lexical.Lexer (alexScanTokens)
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntactic.Parser (happyTokenParse)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Unit Tests" [tokenizerTests, parserTests]

-- these tests are NOT necessarily valid Tiger programs, but only lists
-- of tokens to check that the lexer does what I expect
tokenizerTests = testGroup "Alex-based Tiger lexer" [testLexerARRAY, testLexerArraysID, testLexerArraID, testLexerNIL, testLexerIfThenElse, testLexerIfThenIfThenElse, testLexerWhileDo, testLexerIgnoreComments, testLexerProperForLoop, testLexerBoolArith, testLexerParens, testLexerNestedComments, testStringLiterals, testStringLiteralsWithEscapes, testLexerSubscriptVar, testLexerLetFuncDec]

testLexerARRAY =
  testCase "accepts input 'array' as ARRAY" $ assertEqual [] ([ARRAY]) (alexScanTokens "array")
testLexerArraysID =
  testCase "accepts input 'arrays' as (ID 'arrays')" $ assertEqual [] ([ID "arrays"]) (alexScanTokens "arrays")
testLexerArraID =
  testCase "accepts input 'arra' as (ID 'arra')" $ assertEqual [] ([ID "arra"]) (alexScanTokens "arra")
testLexerNIL =
  testCase "accepts input 'nil' as NIL" $ assertEqual [] ([NIL]) (alexScanTokens "nil")
testLexerIfThenElse =
  testCase "accepts input 'if x then y else z'" $ assertEqual [] ([IF, ID "x", THEN, ID "y", ELSE, ID "z"]) (alexScanTokens "if x then y else z")
testLexerIfThenIfThenElse =
  testCase "accepts input 'if x then if a then b else c'" $ assertEqual [] ([IF, ID "x", THEN, IF, ID "a", THEN, ID "b", ELSE, ID "c"]) (alexScanTokens "if x then if a then b else c")
testLexerWhileDo =
  testCase "accepts input 'while isTrue do 1234'" $ assertEqual [] ([WHILE, ID "isTrue", DO, NUM 1234]) (alexScanTokens "while isTrue do 1234")
testLexerIgnoreComments =
  testCase "accepts input 'for t0 /* some comment */ in someList'" $ assertEqual [] ([FOR, ID "t0", IN, ID "someList"]) (alexScanTokens "for t0 /* some comment */ in someList")
testLexerProperForLoop =
  testCase "accepts input 'for t0 := 0 to 10 do someFunc()'" $ assertEqual [] ([FOR, ID "t0", DEFINE, NUM 0, TO, NUM 10, DO, ID "someFunc", LPAREN, RPAREN]) (alexScanTokens "for t0 := 0 to 10 do someFunc()")
testLexerArith =
  testCase "accepts input 'x = y + 42'" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42]) (alexScanTokens "x = y + 42")
testLexerBoolArith =
  testCase "accepts input 'b := a | b & c;'" $ assertEqual [] ([ID "b", DEFINE, ID "a", PIPE, ID "b", AMPERSAND, ID "c", SEMICOLON]) (alexScanTokens "b := a | b & c;")
testLexerParens =
  testCase "accepts input '12* (42 - 17)/{5}   +a[1]'" $ assertEqual [] ([NUM 12, STAR, LPAREN, NUM 42, MINUS, NUM 17, RPAREN, SLASH, LBRACE, NUM 5, RBRACE, PLUS, ID "a", LBRACKET, NUM 1, RBRACKET]) (alexScanTokens "12* (42 - 17)/{5}   +a[1]")
testLexerNestedComments =
  testCase "accepts input 'x = y /* outer /* inner comment */ outer */ + 42'" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42]) (alexScanTokens "x = y /* outer /* inner comment */ outer */ + 42")
testStringLiterals =
    testCase "accepts input 'if \"string\" = \"other\" then x'" $ assertEqual [] ([IF, STR "string", EQUAL, STR "other", THEN, ID "x"]) (alexScanTokens "if \"string\" = \"other\" then x")
testStringLiteralsWithEscapes =
    testCase "accepts input 'if \"abc\n\" = \"lmn\t\" then x'" $ assertEqual [] ([IF, STR "abc\n", EQUAL, STR "lmn\t", THEN, ID "x"]) (alexScanTokens "if \"abc\n\" = \"lmn\t\" then x")
testLexerSubscriptVar =
  testCase "accepts input 'list[0]'" $ assertEqual [] ([ID "list", LBRACKET, NUM 0, RBRACKET]) (alexScanTokens "list[0]")
testLexerLetFuncDec =
    testCase "accepts input 'let function printboard()...'" $ assertEqual [] ([LET, FUNCTION, ID "printboard", LPAREN, RPAREN, EQUAL, LPAREN, FOR, ID "i", DEFINE, NUM 0, TO, ID "N", MINUS, NUM 1, DO, LPAREN, FOR, ID "j", DEFINE, NUM 0, TO, ID "N", MINUS, NUM 1, DO, ID "print", LPAREN, IF, ID "col", LBRACKET, ID "i", RBRACKET, EQUAL, ID "j", THEN, STR " O", ELSE, STR " .", RPAREN, SEMICOLON, ID "print", LPAREN, STR "\n", RPAREN, RPAREN, SEMICOLON, ID "print", LPAREN, STR "\n", RPAREN, RPAREN, IN, ID "try", LPAREN, NUM 0, RPAREN, END]) (alexScanTokens "let function printboard() = (for i := 0 to N-1 do (for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end")   


-- these tests are NOT necessarily valid Tiger programs, but only syntactically
-- correct constructs to exercise the parser
parserTests = testGroup "Happy-based Tiger parser" [testParserNil, testParserIfThenElse, testParserIfThenIfThenElse, testParserWhileDo, testParserProperForLoop, testParserSubscriptVar, testParserArray, testParserLetVarDecl, testParserLetVarDecl2, testParserLetVarDecl3, testParserLetFuncDecl]

testParserNil =
  testCase "parses 'NIL'" $ assertEqual [] (Program NilExp) (happyTokenParse $ alexScanTokens "nil")
testParserIfThenElse =
  testCase "parses 'if x then y else z'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x")) (VarExp (SimpleVar "y")) (Just (VarExp (SimpleVar "z")))) ) (happyTokenParse $ alexScanTokens "if x then y else z")
testParserIfThenIfThenElse =
  testCase "parses 'if x then if a then b else c'" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x")) (IfExp (VarExp (SimpleVar "a")) (VarExp (SimpleVar "b")) (Just (VarExp (SimpleVar "c")))) (Nothing) ) ) (happyTokenParse $ alexScanTokens "if x then if a then b else c")
testParserWhileDo =
  testCase "parses 'while isTrue do 1234'" $ assertEqual [] (Program (WhileExp (VarExp (SimpleVar "isTrue")) (IntExp 1234))) (happyTokenParse $ alexScanTokens "while isTrue do 1234")
testParserProperForLoop =
  testCase "parses 'for t0 := 0 to 10 do someFunc()'" $ assertEqual [] (Program $ ForExp "t0" True (IntExp 0) (IntExp 10) (CallExp "someFunc" []) ) (happyTokenParse $ alexScanTokens "for t0 := 0 to 10 do someFunc()")
testParserSubscriptVar =
  testCase "parses 'list[0]'" $ assertEqual [] (Program $ VarExp $ SubscriptVar (SimpleVar "list") (IntExp 0)) (happyTokenParse $ alexScanTokens "list[0]")
testParserArray =
  testCase "parses 'int[3+7] of 5'" $ assertEqual [] (Program $ ArrayExp "int" (OpExp (IntExp 3) Add (IntExp 7)) (IntExp 5)) (happyTokenParse $ alexScanTokens "int[3+7] of 5")
testParserLetVarDecl =
  testCase "parses 'let var diag2 := intArray [N+N-1] of 0 in try(0) end'" $ assertEqual [] (Program $ LetExp [VarDecl "diag2" True Nothing (ArrayExp "intArray" (OpExp (OpExp (VarExp (SimpleVar "N")) Add (VarExp (SimpleVar "N"))) Sub (IntExp 1)) (IntExp 0))] (CallExp "try" [IntExp 0])) (happyTokenParse $ alexScanTokens "let var diag2 := intArray [N+N-1] of 0 in try(0) end")
testParserLetVarDecl2 =
  testCase "parses 'let type intArray = array of int in try(0, 1) end'" $ assertEqual [] (Program $ LetExp [TypeDecl "intArray" (ArrayType "int")] (CallExp "try" [IntExp 0, IntExp 1])) (happyTokenParse $ alexScanTokens "let type intArray = array of int in try(0, 1) end")
testParserLetVarDecl3 =
    testCase "parses 'let type any = { any : int } var buffer := getchar()...'" $ assertEqual [] (Program $ LetExp [TypeDecl "any" (RecordType [Field "any" True "int"]), VarDecl "buffer" True Nothing (CallExp "getchar" [])] (CallExp "try" [IntExp 0, IntExp 1])) (happyTokenParse $ alexScanTokens "let type any = { any : int } var buffer := getchar() in try(0, 1) end")
testParserLetFuncDecl =
  testCase "parses 'let function printboard()...'" $ assertEqual [] (Program $ LetExp [FunDecls [FunDecl "printboard" [] Nothing (SeqExp [ForExp "i" True (IntExp 0) (OpExp (VarExp $ SimpleVar "N") Sub (IntExp 1)) (     (SeqExp [ForExp "j" True (IntExp 0) (OpExp (VarExp $ SimpleVar "N") Sub (IntExp 1)) (CallExp "print" [IfExp (OpExp (VarExp $ SubscriptVar (SimpleVar "col") (VarExp $ SimpleVar "i")) Equal (VarExp $ SimpleVar "j")) (StrExp " O") (Just $ StrExp " .")]), (CallExp "print" [StrExp "\n"])])    ), (CallExp "print" [StrExp "\n"])])]] (CallExp "try" [IntExp 0])) (happyTokenParse $ alexScanTokens "let function printboard() = (for i := 0 to N-1 do (for j := 0 to N-1 do print(if col[i]=j then \" O\" else \" .\"); print(\"\n\")); print(\"\n\")) in try(0) end")
                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                        
                                                                                                                                                                                                                                                        
