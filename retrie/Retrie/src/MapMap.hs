{-# LANGUAGE 
    TypeFamilies
  , DeriveFunctor
  , UndecidableInstances
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}

module MapMap where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Logic

import qualified Data.Map as Map
import Control.Applicative hiding (empty)
import qualified Control.Applicative as App

-- TODO: Monads. Or just take 
-- type Rewriter a = TrieMap a (a, Anns)
-- But then how are the anns input?
class Trieable synElmt where
  data TrieMap synElmt :: (* -> *) -> * -> *

  lookup :: Alternative m => synElmt -> TrieMap synElmt m a -> m a
  singleton :: Applicative m => synElmt -> m a -> TrieMap synElmt m a

  empty :: Applicative m => TrieMap synElmt m a
  union :: Alternative m => TrieMap synElmt m a -> TrieMap synElmt m a
    -> TrieMap synElmt m a

  -- ??
  -- alter :: synElmt -> (Maybe a -> Maybe a)
  --   TrieMap synElmt m a -> TrieMap synElmt m a

  -- Q: how to properly capture [], optionality, m??

  -- (current) retrie also takes args: AlphaEnv, Quantifiers
  --- insert :: synElmt -> m a 
  ---   -> TrieMap synElmt m a -> TrieMap synElmt m a
  -- Hang on: Do we need an insert function at all????

    -- Allowing m in the second arg above allows return type to 
    -- read from AlphaEnv, Substitution, (Quantifiers?)

  -- (current) retrie also takes args: AlphaEnv, State(Substitution)
    
    -- Putting [] outside `m a` above will NOT allow success/failure
    -- to depend on Substitution, etc.

-- type M = ListT (TransformerT (State (Subst,AlphaEnv) (Reader Quants (ListT Identity))))
--   or maybe this in reverse order
--   or maybe I should just give up and do everything in JSON

type TransformT = StateT String
type Subst = ()
type AlphaEnv = ()

newtype M a = M { runM :: LogicT (TransformT (StateT (Subst,AlphaEnv) Identity)) a }
type Rewriter a b = TrieMap a M b
type Rewriter' a = TrieMap a M a

type ForkT m a = m (Fork m a)

data Fork m a
  = NoFork (m a)
  | Fair [ForkT m a]
  

----------------------------------------------------------------------

newtype Map k v = Map { unMap :: Map.Map k [v] }
  deriving(Functor, Show, Eq, Ord)

-- instance Ord k => Trieable (Map k)

----------------------------------------------------------------------

newtype Var = Var { unVar :: String }
  deriving(Show, Eq, Ord)

type Liter = Int

data Stmt 
  = Bind Var Expr
  | Run Expr
  | Return Expr
  deriving(Show)

data Expr 
  = Apply Expr Expr
  | Do Stmt Expr
  | LitExpr Liter
  | VarExpr Var
  -- | List [Expr]
  -- | DoList [Stmt]
  deriving(Show)

type R s m a = s -> m a

-- List stuff might need to be baked in somewhere...

instance Trieable Var where
  data TrieMap Var m a = VarTrieMap
    -- { varMap :: Map Var (R Var m a)
    { varMap :: Map.Map Var (R Var m a)
    }
  empty = VarTrieMap Map.empty
  union (VarTrieMap m) (VarTrieMap m')
    = VarTrieMap $ Map.unionWith (\x y v -> x v <|> y v) m m'
  
  singleton var val = VarTrieMap $ Map.singleton var (const val)

  lookup var (VarTrieMap m)
    = case Map.lookup var m of
        Nothing -> App.empty
        Just phi -> phi var

instance Trieable Stmt where
  data TrieMap Stmt m a = StmtTrieMap
    -- { stmtMapBind :: TrieMap Var m (Stmt -> TrieMap Expr m (Stmt -> m a))
    { stmtMapBind :: TrieMap Var m (TrieMap Expr m (R Stmt m a)) -- (Stmt -> m a))
    , stmtMapRun :: TrieMap Expr m (R Stmt m a) -- (Stmt -> m a)
    , stmtMapReturn :: TrieMap Expr m (R Stmt m a) -- (Stmt -> m a)
    }

  empty = StmtTrieMap empty empty empty

  union (StmtTrieMap m0 m1 m2) (StmtTrieMap w0 w1 w2)
    = StmtTrieMap (m0 `union` w0) (m1 `union` w1) (m2 `union` w2)

  singleton (Bind var expr) val 
    = StmtTrieMap (singleton var $ pure (singleton expr $ pure (const val))) empty empty
  singleton (Run expr) val 
    = StmtTrieMap empty (singleton expr $ pure (const val)) empty
  singleton (Return expr) val 
    = StmtTrieMap empty (singleton expr $ pure (const val)) empty

  -- lookup (Bind var expr)

  -- singleton (Bind var expr) val = 

  -- insert (Bind var expr) v map 
  --   = map { stmtMapBind = insert var v (stmtMapBind map) }

instance Trieable Expr where
  data TrieMap Expr m a = ExprTrieMap 
    { exprMapApply :: TrieMap Expr m (TrieMap Expr m (R Expr m a)) -- (Expr -> a))
    , exprMapDo :: TrieMap Stmt m (TrieMap Expr m (R Expr m a)) -- (Expr -> a))
    , exprMapLit :: TrieMap Liter m (R Expr m a) -- (Expr -> a)
    , exprMapVar :: TrieMap Var m (R Expr m a) -- (Expr -> a)
    }
    -- { exprMapApply :: TrieMap Expr m (Expr -> TrieMap Expr m (Expr -> a))
    -- -- , exprMapList :: TrieMap [Expr] a
    -- -- , exprMapDoList :: TrieMap [Stmt] (Expr -> a)
    -- , exprMapDo :: TrieMap Stmt m (Expr -> TrieMap Expr m (Expr -> a))
    -- , exprMapLit :: TrieMap Liter m (Expr -> a)
    -- }


