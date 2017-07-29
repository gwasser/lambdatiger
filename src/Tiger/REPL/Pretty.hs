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
module Tiger.REPL.Pretty (
  ppexpr
) where

import Tiger.Parser.AST

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

parensIf ::  Bool -> Doc -> Doc
parensIf True = PP.parens
parensIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

instance Pretty Expr where
  ppr _ Zero = PP.text "0"
  ppr _ Top = PP.text "true"
  ppr _ Bot = PP.text "false"
  ppr p (Succ a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
  ppr p (Pred a) = (parensIf (p > 0) $ PP.text "succ" <+> ppr (p+1) a)
  ppr p (IsZero a) = (parensIf (p > 0) $ PP.text "iszero" <+> ppr (p+1) a)
  ppr p (If a b c) =
        PP.text "if"   <+> ppr p a
    <+> PP.text "then" <+> ppr p b
    <+> PP.text "else" <+> ppr p c

ppexpr :: Expr -> String
ppexpr = PP.render . ppr 0 
