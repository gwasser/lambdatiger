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

import Tiger.Lexical.Lexer (alexMonadScanTokens)
import Tiger.Lexical.Tokens (Token(..))
import Tiger.Syntactic.Parser (happyTokenParse)
import Tiger.Syntactic.AST (Program(..), Exp(..), Var(..), Decl(..), Type(..), Op(..), Symbol, Field(..), FunDecl(..))
                             
-- these tests are based on the ones written by Appel in sample ML code,
-- and distributed via the textbook website
appelTests = testGroup "Appel's original tests for Tiger" [testParseAppel1, testParseAppel2, testParseAppel3, testParseAppel4, testParseAppel5, testParseAppel6, testParseAppel7, testParseAppel8, testParseAppel9, testParseAppel10, testParseAppel11, testParseAppel12, testParseAppel13, testParseAppel14, testLexAppel15, testParseAppel15, testParseAppel16, testParseAppel17, testParseAppel18, testParseAppel19, testParseAppel20]

-- TODO: maybe use testCaseSteps in future when do typechecking?
-- (https://hackage.haskell.org/package/tasty-hunit-0.9.2/docs/Test-Tasty-HUnit.html)

testParseAppel1 =
    testCase "parses test1 (read from file)" $ (readFile "test/appeltestcases/test1.tig" >>= (assertEqual [] (Program $ LetExp [TypeDecl "arrtype" (ArrayType "int"), VarDecl "arr1" True (Just "arrtype") (ArrayExp "arrtype" (IntExp 10) (IntExp 0))] (VarExp $ SimpleVar "arr1"))) . happyTokenParse . alexMonadScanTokens)
testParseAppel2 =
    testCase "parses test2 (read from file)" $ (readFile "test/appeltestcases/test2.tig" >>= (assertEqual [] (Program $ LetExp [TypeDecl "myint" (NameType "int"), TypeDecl "arrtype" (ArrayType "myint"), VarDecl "arr1" True (Just "arrtype") (ArrayExp "arrtype" (IntExp 10) (IntExp 0))] (VarExp $ SimpleVar "arr1"))) . happyTokenParse . alexMonadScanTokens)
testParseAppel3 =
    testCase "parses test3 (read from file)" $ (readFile "test/appeltestcases/test3.tig" >>= (assertEqual [] (Program $ LetExp [TypeDecl "rectype" (RecordType [Field "name" True "string", Field "age" True "int"]), VarDecl "rec1" True (Just "rectype") (RecordExp [("age", IntExp 1000), ("name", StrExp "Nobody")] ("rectype"))] (SeqExp [AssignExp (FieldVar (SimpleVar "rec1") "name") (StrExp "Somebody"), VarExp $ SimpleVar "rec1"]))) . happyTokenParse . alexMonadScanTokens)
testParseAppel4 =
    testCase "parses test4 (read from file)" $ (readFile "test/appeltestcases/test4.tig" >>= (assertEqual [] (Program (LetExp {decls = [FunDecls [FunDecl {fundeclname = "nfactor", params = [Field {fieldname = "n", escape = True, typ = "int"}], result = Just "int", body = IfExp {iftest = OpExp {left = VarExp (SimpleVar "n"), oper = Equal, right = IntExp 0}, thenexp = IntExp 1, elseexp = Just (OpExp {left = VarExp (SimpleVar "n"), oper = Mul, right = CallExp {func = "nfactor", args = [OpExp {left = VarExp (SimpleVar "n"), oper = Sub, right = IntExp 1}]}})}}]], lbody = CallExp {func = "nfactor", args = [IntExp 10]}}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel5 =
    testCase "parses test5 (read from file)" $ (readFile "test/appeltestcases/test5.tig" >>= (assertEqual [] (Program (LetExp {decls = [TypeDecl {tname = "intlist", ttyp = RecordType [Field {fieldname = "hd", escape = True, typ = "int"},Field {fieldname = "tl", escape = True, typ = "intlist"}]}, TypeDecl {tname = "tree", ttyp = RecordType [Field {fieldname = "key", escape = True, typ = "int"},Field {fieldname = "children", escape = True, typ = "treelist"}]},TypeDecl {tname = "treelist", ttyp = RecordType [Field {fieldname = "hd", escape = True, typ = "tree"},Field {fieldname = "tl", escape = True, typ = "treelist"}]},VarDecl {vname = "lis", vescape = True, vtyp = Just "intlist", vinit = RecordExp {fields = [("tl",NilExp),("hd",IntExp 0)], rtyp = "intlist"}}], lbody = VarExp (SimpleVar "lis")}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel6 =
    testCase "parses test6 (read from file)" $ (readFile "test/appeltestcases/test6.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [FunDecls [FunDecl {fundeclname = "do_nothing1", params = [Field {fieldname = "a", escape = True, typ = "int"},Field {fieldname = "b", escape = True, typ = "string"}], result = Nothing, body = CallExp {func = "do_nothing2", args = [OpExp {left = VarExp (SimpleVar "a"), oper = Add, right = IntExp 1}]}},FunDecl {fundeclname = "do_nothing2", params = [Field {fieldname = "d", escape = True, typ = "int"}], result = Nothing, body = CallExp {func = "do_nothing1", args = [VarExp (SimpleVar "d"),StrExp "str"]}}]], lbody = CallExp {func = "do_nothing1", args = [IntExp 0,StrExp "str2"]}}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel7 =
    testCase "parses test7 (read from file)" $ (readFile "test/appeltestcases/test7.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [FunDecls [FunDecl {fundeclname = "do_nothing1", params = [Field {fieldname = "a", escape = True, typ = "int"},Field {fieldname = "b", escape = True, typ = "string"}], result = Just "int", body = SeqExp [CallExp {func = "do_nothing2", args = [OpExp {left = VarExp (SimpleVar "a"), oper = Add, right = IntExp 1}]},IntExp 0]},FunDecl {fundeclname = "do_nothing2", params = [Field {fieldname = "d", escape = True, typ = "int"}], result = Just "string", body = SeqExp [CallExp {func = "do_nothing1", args = [VarExp (SimpleVar "d"),StrExp "str"]},StrExp " "]}]], lbody = CallExp {func = "do_nothing1", args = [IntExp 0,StrExp "str2"]}}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel8 =
    testCase "parses test8 (read from file)" $ (readFile "test/appeltestcases/test8.tig" >>= (assertEqual [] (Program $ IfExp {iftest = OpExp {left = IntExp 10, oper = GreaterThan, right = IntExp 20}, thenexp = IntExp 30, elseexp = Just $ IntExp 40})) . happyTokenParse . alexMonadScanTokens)
testParseAppel9 =
    testCase "parses test9 (read from file)" $ (readFile "test/appeltestcases/test9.tig" >>= (assertEqual [] (Program $ IfExp {iftest = OpExp {left = IntExp 5, oper = GreaterThan, right = IntExp 4}, thenexp = IntExp 13, elseexp = Just $ StrExp " "})) . happyTokenParse . alexMonadScanTokens)
testParseAppel10 =
    testCase "parses test10 (read from file)" $ (readFile "test/appeltestcases/test10.tig" >>= (assertEqual [] (Program $ WhileExp {wtest = OpExp {left = IntExp 10, oper = GreaterThan, right = IntExp 5}, wbody = OpExp {left = IntExp 5, oper = Add, right = IntExp 6}})) . happyTokenParse . alexMonadScanTokens)
testParseAppel11 =
    testCase "parses test11 (read from file)" $ (readFile "test/appeltestcases/test11.tig" >>= (assertEqual [] (Program $ ForExp { fvar = "i", fescape = True, lo = IntExp 10, hi = StrExp " ", fbody = AssignExp { avar = SimpleVar "i", aexp = OpExp {left = VarExp $ SimpleVar "i", oper = Sub, right = IntExp 1} } })) . happyTokenParse . alexMonadScanTokens)
testParseAppel12 =
    testCase "parses test12 (read from file)" $ (readFile "test/appeltestcases/test12.tig" >>= (assertEqual [] (Program $ LetExp [VarDecl "a" True Nothing (IntExp 0)] (ForExp "i" True (IntExp 0) (IntExp 100) (SeqExp [AssignExp (SimpleVar "a") (OpExp (VarExp $ SimpleVar "a") Add (IntExp 1)), SeqExp []])))) . happyTokenParse . alexMonadScanTokens)
testParseAppel13 =
    testCase "parses test13 (read from file)" $ (readFile "test/appeltestcases/test13.tig" >>= (assertEqual [] (Program $ OpExp {left = IntExp 3, oper = GreaterThan, right = StrExp "df"})) . happyTokenParse . alexMonadScanTokens)
testParseAppel14 =
    testCase "parses test14 (read from file)" $ (readFile "test/appeltestcases/test14.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [TypeDecl {tname = "arrtype", ttyp = ArrayType "int"},TypeDecl {tname = "rectype", ttyp = RecordType [Field {fieldname = "name", escape = True, typ = "string"},Field {fieldname = "id", escape = True, typ = "int"}]},VarDecl {vname = "rec", vescape = True, vtyp = Nothing, vinit = RecordExp {fields = [("id",IntExp 0),("name",StrExp "aname")], rtyp = "rectype"}},VarDecl {vname = "arr", vescape = True, vtyp = Nothing, vinit = ArrayExp {atyp = "arrtype", size = IntExp 3, ainit = IntExp 0}}], lbody = IfExp {iftest = OpExp {left = VarExp (SimpleVar "rec"), oper = NotEqual, right = VarExp (SimpleVar "arr")}, thenexp = IntExp 3, elseexp = Just (IntExp 4)}}))) . happyTokenParse . alexMonadScanTokens)
testLexAppel15 =
    testCase "lexes test15 (read from file)" $ (readFile "test/appeltestcases/test15.tig" >>= (assertEqual [] ([IF, NUM 20, THEN, NUM 3, TEOF])) . alexMonadScanTokens)
testParseAppel15 =
    testCase "parses test15 (read from file)" $ (readFile "test/appeltestcases/test15.tig" >>= (assertEqual [] (Program $ IfExp {iftest = IntExp 20, thenexp = IntExp 3, elseexp = Nothing})) . happyTokenParse . alexMonadScanTokens)
testParseAppel16 =
    testCase "parses test16 (read from file)" $ (readFile "test/appeltestcases/test16.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [TypeDecl {tname = "a", ttyp = NameType "c"},TypeDecl {tname = "b", ttyp = NameType "a"},TypeDecl {tname = "c", ttyp = NameType "d"},TypeDecl {tname = "d", ttyp = NameType "a"}], lbody = StrExp ""}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel17 =
    testCase "parses test17 (read from file)" $ (readFile "test/appeltestcases/test17.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [TypeDecl {tname = "tree", ttyp = RecordType [Field {fieldname = "key", escape = True, typ = "int"},Field {fieldname = "children", escape = True, typ = "treelist"}]},VarDecl {vname = "d", vescape = True, vtyp = Just "int", vinit = IntExp 0},TypeDecl {tname = "treelist", ttyp = RecordType [Field {fieldname = "hd", escape = True, typ = "tree"},Field {fieldname = "tl", escape = True, typ = "treelist"}]}], lbody = VarExp (SimpleVar "d")}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel18 =
    testCase "parses test18 (read from file)" $ (readFile "test/appeltestcases/test18.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [FunDecls [FunDecl {fundeclname = "do_nothing1", params = [Field {fieldname = "a", escape = True, typ = "int"},Field {fieldname = "b", escape = True, typ = "string"}], result = Just "int", body = SeqExp [CallExp {func = "do_nothing2", args = [OpExp {left = VarExp (SimpleVar "a"), oper = Add, right = IntExp 1}]},IntExp 0]}],VarDecl {vname = "d", vescape = True, vtyp = Nothing, vinit = IntExp 0},FunDecls [FunDecl {fundeclname = "do_nothing2", params = [Field {fieldname = "d", escape = True, typ = "int"}], result = Just "string", body = SeqExp [CallExp {func = "do_nothing1", args = [VarExp (SimpleVar "d"),StrExp "str"]},StrExp " "]}]], lbody = CallExp {func = "do_nothing1", args = [IntExp 0,StrExp "str2"]}}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel19 =
    testCase "parses test19 (read from file)" $ (readFile "test/appeltestcases/test19.tig" >>= (assertEqual [] (Program $ (LetExp {decls = [FunDecls [FunDecl {fundeclname = "do_nothing1", params = [Field {fieldname = "a", escape = True, typ = "int"},Field {fieldname = "b", escape = True, typ = "string"}], result = Just "int", body = SeqExp [CallExp {func = "do_nothing2", args = [OpExp {left = VarExp (SimpleVar "a"), oper = Add, right = IntExp 1}]},IntExp 0]},FunDecl {fundeclname = "do_nothing2", params = [Field {fieldname = "d", escape = True, typ = "int"}], result = Just "string", body = SeqExp [CallExp {func = "do_nothing1", args = [VarExp (SimpleVar "a"),StrExp "str"]},StrExp " "]}]], lbody = CallExp {func = "do_nothing1", args = [IntExp 0,StrExp "str2"]}}))) . happyTokenParse . alexMonadScanTokens)
testParseAppel20 =
    testCase "parses test20 (read from file)" $ (readFile "test/appeltestcases/test20.tig" >>= (assertEqual [] (Program $ (WhileExp {wtest = OpExp {left = IntExp 10, oper = GreaterThan, right = IntExp 5}, wbody = SeqExp [OpExp {left = VarExp (SimpleVar "i"), oper = Add, right = IntExp 1},SeqExp []]}))) . happyTokenParse . alexMonadScanTokens)
    
-- testParseAppel6 =
    --testCase "parses test6 (read from file)" $ (readFile "test/appeltestcases/test6.tig" >>= (assertEqual [] (Program $ NilExp)) . happyTokenParse . alexMonadScanTokens)
    
