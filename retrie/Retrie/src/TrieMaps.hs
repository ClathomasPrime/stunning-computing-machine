{-# LANGUAGE 
    TypeFamilies
  , DeriveFunctor
  , UndecidableInstances
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , FlexibleContexts 
  #-}
import qualified Data.Map as Map


type Subst = ()

newtype Map k v = Map { unMap :: Map.Map k [v] }
  deriving(Functor, Show)

data Leaf synElmt result
  = LeafTemplate (Template synElmt result)
  | LeafTemplateThen 
      (Template synElmt result) -- or MatchingState -> synElmt
      -- (MatchingState -> MatchingState) -- don't need this now
      (TrieMap synElmt (Template synElmt result))
  | LeafNil

data MatchingState = MatchingState 
  { matchingQuant :: ()
  }

data Rewriter synElmt result = TrieMap synElmt (Leaf synElmt result)

type TemplateFor x = MatchingState -> x

class Trieable synElmt where
  data TrieMap synElmt :: * -> *

  data Template synElmt :: * -> *

  alphaEquiv :: synElmt -> synElmt -> Bool

  mIsEmpty :: TrieMap synElmt a -> Bool
  mEmpty :: TrieMap synElmt a
  mUnion :: TrieMap synElmt a -> TrieMap synElmt a -> TrieMap synElmt a

  rewrite :: synElmt -> TrieMap synElmt (Template synElmt result) -> result
  -- mInsert :: synElmt -> 
  -- mMatch :: synElmt -> 

  mAlter :: synElmt -> (Maybe a -> Maybe a) 
    -> TrieMap synElmt a -> TrieMap synElmt a
  mMatch :: Subst -> synElmt -> TrieMap synElmt a -> [(Subst, a)]

instance Trieable () where
  data TrieMap () a 
    = LeafTrieMap { unLeafTrieMap :: [a] }

  mEmpty = LeafTrieMap []
  mUnion (LeafTrieMap as) (LeafTrieMap as') = LeafTrieMap $ as ++ as'

instance Trieable [synElmt] where
  data TrieMap [synElmt] a = ListTrieMap
    { listMapNil :: TrieMap () a
    , listMapCons :: TrieMap synElmt (TrieMap [synElmt] a)
    }
  

data Hole a = Hole a

instance Trieable a => Trieable (Hole a) where
