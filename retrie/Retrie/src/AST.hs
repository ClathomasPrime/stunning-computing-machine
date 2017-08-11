module AST where

data L t = L String t

-- data Module = Module
--   { modName :: L String
--   , mod

newtype Var = Var { unVar :: String }
  deriving(Show, Eq, Ord)

type Liter = Int

data Stmt 
  = Bind (L Var) (L Expr)
  | Run (L Expr)
  | Return (L Expr)
  deriving(Show)

data Expr 
  = Apply (L Expr) (L Expr)
  | Do (L Stmt) (L Expr)
  | LitExpr (L Liter)
  | VarExpr (L Var)
  -- | List [Expr]
  -- | DoList [Stmt]
  deriving(Show)
