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

module SemantTest where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (assertEqual, testCase, testCaseSteps)

import Tiger.Lexical.Lexer (alexMonadScanTokens, alexMonadScanTokensWithPosn)
import Tiger.Lexical.Tokens (Token(..), L(..), AlexPosn(..))
import Tiger.Syntactic.Parser (happyTokenParse, happyTokenParseWithPosn)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDec(..))
import Tiger.Semantic.RedBlackTree (RedBlackTree(..), Color(..), empty, member, insert)


-- Tiger programs that we can check the semantics of
redblackTests = testGroup "Red-Black Trees" [testRB]

testRB =
  testCaseSteps "simple tree ops with insert and member" $ \step -> do
      step "empty tree"
      let rbEmpty = empty :: RedBlackTree Int
      assertEqual [] (E) (rbEmpty)
      
      step "insert 5"
      let rbStep2 = insert 5 rbEmpty
      assertEqual [] (T B E 5 E) (rbStep2)
      
      step "insert 7"
      let rbStep3 = insert 7 rbStep2
      assertEqual [] (T B E 5 (T R E 7 E)) (rbStep3)
      
      step "insert 9"
      let rbStep4 = insert 9 rbStep3
      assertEqual [] (T B (T B E 5 E) 7 (T B E 9 E)) (rbStep4)
      
      step "insert 4, 6"
      let rbStep5 = insert 4 (insert 6 rbStep4)
      assertEqual [] (T B (T B (T R E 4 E) 5 (T R E 6 E)) 7 (T B E 9 E)) (rbStep5)
      
      step "insert 8, 10"
      let rbStep6 = insert 10 (insert 8 rbStep5)
      assertEqual [] (T B (T B (T R E 4 E) 5 (T R E 6 E)) 7 (T B (T R E 8 E) 9 (T R E 10 E))) (rbStep6)
      
      step "insert 3"
      let rbStep7 = insert 3 rbStep6
      assertEqual [] (T B (T R (T B E 3 E) 4 (T B E 5 (T R E 6 E))) 7 (T B (T R E 8 E) 9 (T R E 10 E))) (rbStep7)
      
      step "member 6? yes"
      assertEqual [] (True) (member 6 rbStep7)
      
      step "member 42? no"
      assertEqual [] (False) (member 42 rbStep7)


                                                                                                                                                                                                                                                        
                             
