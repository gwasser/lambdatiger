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

tokenizerTests = testGroup "Alex-based Tiger lexer" [testLexerARRAY, testLexerArraysID, testLexerArraID, testLexerNIL, testLexerIfThenElse, testLexerWhileDo, testLexerIgnoreComments, testLexerProperForLoop, testLexerBoolArith, testLexerParens, testLexerNestedComments, testStringLiterals]

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
  testCase "accepts input 'x = y /* outer /* inner comment */ outer */ + 42'" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42]) (alexScanTokens "x = y /* outer /* some inner comment */ outer */ + 42")
testStringLiterals =
    testCase "accepts input 'if \"string\" = \"other\" then x']" $ assertEqual [] ([IF, STR "string", EQUAL, STR "other", THEN, ID "x"]) (alexScanTokens "if \"string\" = \"other\" then x")



parserTests = testGroup "Happy-based Tiger parser" [testParserNil, testParserIfThenElse, testParserWhileDo, testParserProperForLoop]

testParserNil =
  testCase "parses 'NIL' program" $ assertEqual [] (Program NilExp) (happyTokenParse $ alexScanTokens "nil")
testParserIfThenElse =
  testCase "parses 'if x then y else z' program" $ assertEqual [] (Program (IfExp (VarExp (SimpleVar "x")) (VarExp (SimpleVar "y")) (Just (VarExp (SimpleVar "z")))) ) (happyTokenParse $ alexScanTokens "if x then y else z")
testParserWhileDo =
  testCase "parses 'while isTrue do 1234' program" $ assertEqual [] (Program (WhileExp (VarExp (SimpleVar "isTrue")) (IntExp 1234))) (happyTokenParse $ alexScanTokens "while isTrue do 1234")
testParserProperForLoop =
  testCase "parses 'for t0 := 0 to 10 do someFunc()' program" $ assertEqual [] (Program $ ForExp "t0" True (IntExp 0) (IntExp 10) (CallExp "someFunc" []) ) (happyTokenParse $ alexScanTokens "for t0 := 0 to 10 do someFunc()")


