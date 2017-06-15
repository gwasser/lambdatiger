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

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Tiger.Lexer.RegEx (Regex(..))
import Tiger.Lexer.NFA (SymbolOrEmpty(..), State(..), NFA, regexToNFA)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit Tests for RE -> NFA"
    [testEmptyRE, testSingleCharRE]

emptyRE = [(((Initial 0), Empty), [(Final 1)])]
testEmptyRE =
  testCase "empty" $ assertEqual [] emptyRE (regexToNFA (Epsilon))

justaRE = [(((Initial 0), Symbol 'a'), [(Final 1)])]
testSingleCharRE =
  testCase "a" $ assertEqual [] justaRE (regexToNFA (Exact 'a'))
