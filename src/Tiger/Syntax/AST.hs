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

module Tiger.Syntax.AST where

-- a Tiger program is simply an expression
data Program = Program Exp
                deriving Show

-- appendix A.3 of Appel defines Expressions:
data Exp  
      = Nil
      | AssignArray Var Exp Exp
      | AssignVar LVal Exp
      | IfElse Exp Exp Exp
      | If Exp Exp
      | While Exp Exp
      | For Var Exp Exp Exp
      | Break
      | Let DeclList Exp
      | ArithExp ArithExp
      | BoolExp BoolExp
      | CompareExp CompareExp
      deriving Show
      
type Var = String
      
-- |DeclList is either a single Decl, or
-- a DeclList followed by one more Decl
-- (recursive so we have list of any size)
data DeclList
      = Decl Declaration
      | List DeclList Declaration
      deriving Show

data Declaration
      = TyDecl TypeName Type
      | VarDecl VarDecl
      | FunDecl FunDecl
      deriving Show
      
type TypeName = String
      
data Type
    = Alias TypeName
    | TypeFields TypeFields
    | Array TypeName
    deriving Show
    
data TypeFields
    = Empty
    | Field Var TypeName
    | FieldList Var TypeName TypeFields
    deriving Show
    
data VarDecl
    = InitVar Var Exp
    | InitVarType Var TypeName Exp
    deriving Show
    
data FunDecl
    = FunDef Var TypeFields Exp
    | FunDefType Var TypeFields TypeName Exp
    deriving Show
    
data LVal
    = VarVal Var
    | FieldVal LVal Var
    | ArrayVal LVal Exp
    deriving Show

data ArithExp 
      = Add ArithExp Term 
      | Sub ArithExp Term 
      | Term Term
      deriving Show

data Term 
      = Mul Term Factor 
      | Div Term Factor 
      | Factor Factor
      deriving Show

data Factor 
      = Int Int 
      | Var String 
      | AParen ArithExp
      deriving Show
      
data CompareExp
    = Equal Exp Exp
    | NotEqual Exp Exp
    | GreaterThan Exp Exp
    | LessThan Exp Exp
    | GreaterEqual Exp Exp
    | LessEqual Exp Exp
    deriving Show
    
data BoolExp
    = Or BoolExp BTerm
    | BTerm BTerm
    deriving Show
    
data BTerm
    = And BTerm BoolExp
    | BParen BoolExp
    deriving Show
