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

module Tiger.Syntactic.AST where

-- a Tiger program is simply an expression
data Program = Program Exp
                deriving (Show, Eq)

-- appendix A.3 of Appel defines Expressions,
-- figure 4.8 gives suggested abstract syntax
data Exp  
      = NilExp
      | VarExp Var
      | IntExp Int
      | StrExp String
      | CallExp { func :: Symbol, args :: [Exp] }
      | OpExp { left :: Exp, oper :: Op, right :: Exp }
      | RecordExp { fields :: [(Symbol,Exp)], rtyp :: Symbol }
      | SeqExp [Exp]
      | AssignExp { avar :: Var, aexp :: Exp }
      | IfExp { iftest :: Exp, thenexp :: Exp, elseexp :: Maybe Exp }
      | WhileExp { wtest :: Exp, wbody :: Exp }
      | ForExp { fvar :: Symbol, fescape :: Bool, lo :: Exp, hi :: Exp, fbody :: Exp }
      | BreakExp
      | LetExp { decls :: [Decl], lbody :: Exp }
      | ArrayExp { atyp :: Symbol, size :: Exp, ainit :: Exp }
      deriving (Show, Eq)
      
data Var
    = SimpleVar Symbol
    | FieldVar Var Symbol
    | SubscriptVar Var Exp
    deriving (Show, Eq)
    
data Decl
    = FunDecls [FunDecl]
    | VarDecl { vname :: Symbol, vescape :: Bool, vtyp :: Maybe Symbol, vinit :: Exp }
    | TypeDecl { tname :: Symbol, ttyp :: Type }
    deriving (Show, Eq)
    
data Type
    = NameType Symbol
    | RecordType [Field]
    | ArrayType Symbol
    deriving (Show, Eq)
    
data Op
    = Add
    | Sub
    | Mul
    | Div
    | Equal
    | NotEqual
    | GreaterThan
    | LessThan
    | GreaterEqual
    | LessEqual
    deriving (Show, Eq)
    
type Symbol = String

data Field = Field { fieldname :: Symbol, escape :: Bool, typ :: Symbol } deriving (Show, Eq)
      
data FunDecl = FunDecl { fundeclname :: Symbol, params :: [Field], result :: Maybe Symbol, body :: Exp } deriving (Show, Eq)
