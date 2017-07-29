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
module Tiger.REPL.Interpreter
    ( eval
    ) where

import Control.Applicative
import Data.Maybe

import Tiger.Parser.AST

isNum :: Expr -> Bool
isNum Zero = True
isNum (Succ t) = isNum t
isNum _ = False

isVal :: Expr -> Bool
isVal Top = True
isVal Bot = True
isVal t | isNum t = True
isVal _ = False

eval' x = case x of
               IsZero Zero -> Just Top
               IsZero (Succ t) | isNum t -> Just Bot
               IsZero t -> IsZero <$> (eval' t)
               Succ t -> Succ <$> (eval' t)
               Pred Zero -> Just Zero
               Pred (Succ t) | isNum t -> Just t
               Pred t -> Pred <$> (eval' t)
               If Top c _ -> Just c
               If Bot _ a -> Just a
               If t c a -> (\t' -> If t' c a) <$> eval' t
               _ -> Nothing
               
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expr -> Maybe Expr
eval t = case nf t of
              nft | isVal nft -> Just nft
                  | otherwise -> Nothing -- term is "stuck"
          
evalArith :: ArithExpr -> Int
evalArith ex = case ex of
               Add a b -> evalArith a + evalArith b
               Mul a b -> evalArith a * evalArith b
               Sub a b -> evalArith a - evalArith b
               Lit n   -> n 
