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

import Tiger.Lexer.Tokenizer (alexScanTokens)
import Tiger.Lexer.Tokens (Token(..))

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Unit Tests" [tokenizerTests]

tokenizerTests = testGroup "Unit Tests for Alex-based Tiger Lexer" [testLexerARRAY, testLexerArraysID, testLexerArraID, testLexerIfThenElse, testLexerWhileDo, testLexerIgnoreComments, testLexerBoolArith, testLexerParens]

testLexerARRAY =
  testCase "accepts input 'array' as ARRAY" $ assertEqual [] (ARRAY) (head $ alexScanTokens "array")
testLexerArraysID =
  testCase "accepts input 'arrays' as (ID 'arrays')" $ assertEqual [] (ID "arrays") (head $ alexScanTokens "arrays")
testLexerArraID =
  testCase "accepts input 'arra' as (ID 'arra')" $ assertEqual [] (ID "arra") (head $ alexScanTokens "arra")
testLexerIfThenElse =
  testCase "accepts input 'if x then y else z' as [IF, ID 'x', THEN, ID 'y', ELSE, ID 'z']" $ assertEqual [] ([IF, ID "x", THEN, ID "y", ELSE, ID "z"]) (alexScanTokens "if x then y else z")
testLexerWhileDo =
  testCase "accepts input 'while isTrue do 1234' as [WHILE, ID 'isTrue', DO, NUM 1234]" $ assertEqual [] ([WHILE, ID "isTrue", DO, NUM 1234]) (alexScanTokens "while isTrue do 1234")
testLexerIgnoreComments =
  testCase "accepts input 'for t0 /* some comment */ in someList' as [FOR, ID 't0', IN, ID 'someList']" $ assertEqual [] ([FOR, ID "t0", IN, ID "someList"]) (alexScanTokens "for t0 /* some comment */ in someList")
testLexerArith =
  testCase "accepts input 'x = y + 42' as [ID 'x', EQUAL, ID 'y', PLUS, NUM 42]" $ assertEqual [] ([ID "x", EQUAL, ID "y", PLUS, NUM 42]) (alexScanTokens "x = y + 42")
testLexerBoolArith =
  testCase "accepts input 'b := a | b & c;' as [ID 'b', DEFINE, ID 'a', PIPE, ID 'b', AMPERSAND, ID 'c', SEMICOLON]" $ assertEqual [] ([ID "b", DEFINE, ID "a", PIPE, ID "b", AMPERSAND, ID "c", SEMICOLON]) (alexScanTokens "b := a | b & c;")
testLexerParens =
  testCase "accepts input '12* (42 - 17)/{5}   +a[1]' as [NUM 12, STAR, LPAREN, NUM 42, MINUS, NUM 17, RPAREN, SLASH, LBRACE, NUM 5, RBRACE, PLUS, ID 'a', LBRACKET, NUM 1, RBRACKET]" $ assertEqual [] ([NUM 12, STAR, LPAREN, NUM 42, MINUS, NUM 17, RPAREN, SLASH, LBRACE, NUM 5, RBRACE, PLUS, ID "a", LBRACKET, NUM 1, RBRACKET]) (alexScanTokens "12* (42 - 17)/{5}   +a[1]")
--testtigerDFABreak1 =
--  testCase "tigerDFA should NOT accept input 'br+eak'" $ assertEqual [] False (alexScanTokens "br+eak")



