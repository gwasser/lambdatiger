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

module LexerTest where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tiger.Lexical.Lexer (scanner)
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntactic.Parser (happyTokenParse)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))

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
                                                                                                                                                                                                                            
