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


{- Based on the NanoParsec parser in "Write You a Haskell" by Stephen Diehl -}
module Tiger.Parser.Syntax where

-- name of a variable
type Name = String

data ArithExpr = Add ArithExpr ArithExpr
          | Mul ArithExpr ArithExpr
          | Sub ArithExpr ArithExpr
          | Lit Int
          deriving Show 
          
data Expr = Top -- Top, or True
          | Bot -- Bottom, or False
          | Zero
          | IsZero Expr
          | Succ Expr
          | Pred Expr
          | If Expr Expr Expr
          deriving (Eq, Show)
