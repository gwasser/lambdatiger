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

import Tiger.Lexer.DFA (DFA, runDFA, execDFA)
import Tiger.Lexer.Tokens (Token(..))
import Tiger.Lexer.Tokenizer (TokenState(..), tigerTokenizer)

main = defaultMain tests

tests :: TestTree
tests = testGroup "All Unit Tests" [dfaTests, tokenizerTests]

tokenizerTests = testGroup "Unit Tests for Tokenizer" [testTokenizerArray1, testTokenizerArray2, testTokenizerArray3, testTokenizerArray4, testTokenizerArray5, testTokenizerArray6, testTokenizerBreak1]

testTokenizerArray1 =
  testCase "see if Tokenizer accepts input 'array'" $ assertEqual [] True (execDFA tigerTokenizer "array")
testTokenizerArray2 =
  testCase "see if Tokenizer accepts input 'array' in state ARRAY" $ assertEqual [] (TokenState ARRAY,([],"array")) (runDFA tigerTokenizer "array")
testTokenizerArray3 =
  testCase "see if Tokenizer accepts input 'arrays'" $ assertEqual [] True (execDFA tigerTokenizer "arrays")
testTokenizerArray4 =
  testCase "see if Tokenizer accepts input 'arrays' in state (ID 'arrays')" $ assertEqual [] (TokenState (ID "arrays"),([],"arrays")) (runDFA tigerTokenizer "arrays")
testTokenizerArray5 =
  testCase "see if Tokenizer accepts input 'arra'" $ assertEqual [] True (execDFA tigerTokenizer "arra")
testTokenizerArray6 =
  testCase "see if Tokenizer accepts input 'arra' in state (ID 'arra')" $ assertEqual [] (TokenState (ID "arra"),([],"arra")) (runDFA tigerTokenizer "arra")
testTokenizerBreak1 =
  testCase "Tokenizer should NOT accept input 'br+eak'" $ assertEqual [] False (execDFA tigerTokenizer "br+eak")


dfaTests = testGroup "Unit Tests for DFAs" [testExecDFA1,testRunDFA1,testExecDFA2]

-- example of DFA for RE (a|b)*abb, with input String of "ababb",
-- from Example 3.19, page 150, of Aho et al.
dfa1 = (dfa1st, dfa1alphabet, dfa1delta, 0, dfa1inF) :: DFA Int
dfa1st = [0,1,2,3]
dfa1alphabet = ['a','b']
dfa1delta 0 'a' = 1
dfa1delta 0 'b' = 0
dfa1delta 1 'a' = 1
dfa1delta 1 'b' = 2
dfa1delta 2 'a' = 1
dfa1delta 2 'b' = 3
dfa1delta 3 'a' = 1
dfa1delta 3 'b' = 0
dfa1inF 3 = True
dfa1inF _ = False
testExecDFA1 =
  testCase "see if DFA for (a|b)*abb accepts input 'ababb'" $ assertEqual [] True (execDFA dfa1 "ababb")
testRunDFA1 =
  testCase "see if DFA for (a|b)*abb accepts input 'ababb' in state 3" $ assertEqual [] (3,([],"ababb")) (runDFA dfa1 "ababb")
testExecDFA2 =
  testCase "DFA for (a|b)*abb should NOT accept input 'ababaa'" $ assertEqual [] False (execDFA dfa1 "ababaa")


