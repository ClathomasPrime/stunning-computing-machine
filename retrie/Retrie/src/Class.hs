{-# LANGUAGE 
    TypeFamilies
  , DeriveFunctor
  , UndecidableInstances
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  #-}

module Class where

import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Logic

import qualified Data.Map as Map
import Control.Applicative hiding (empty)
import qualified Control.Applicative as App


type TransformT = StateT Anns
type Anns = Map.Map String (Int,Int,String)
type Subst = ()
type AlphaEnv = ()

newtype M a 
  = M { runM :: LogicT (TransformT (StateT (Subst,AlphaEnv) Identity)) a }
type Rewriter a b = TrieMap a M b
type Rewriter' a = TrieMap a M a

--------------------------------------------------------------------------------

class Trieable synElmt where
  data TrieMap synElmt :: (* -> *) -> * -> *

  lookup :: Alternative m => synElmt -> TrieMap synElmt m a -> m a
  singleton :: Applicative m => synElmt -> m a -> TrieMap synElmt m a

  empty :: Applicative m => TrieMap synElmt m a
  union :: Alternative m => 
    TrieMap synElmt m a -> TrieMap synElmt m a -> TrieMap synElmt m a

--------------------------------------------------------------------------------

