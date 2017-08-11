module AST where

import Data.Map (Map)
import qualified Data.Map as Map

data Decl 
  = Define Var Expr
  | Main Expr

data Expr
  = If Expr Expr Expr
  | Block [Expr]
  | While Expr Expr

data Value
  = ValInt Int
  | ValPair Location Location

type Var = String

type Location = Int

type Heap = Map Location Value

type Program = ([Thread], Heap)

data Thread = Thread
  { context :: ExprZipper
  , namespace :: Map Var Location
  }

smallStep :: Program -> m Program
