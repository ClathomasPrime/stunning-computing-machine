{-# LANGUAGE Rank2Types #-}

import Unsafe.Coerce
import Control.Monad.Identity
import Control.Applicative

data TyRep = TyRep String [TyRep]
  deriving(Show, Eq)

class Typeable a where
  typeOf :: a -> TyRep

class Typeable a => Term a where
  gmapT :: (forall b. Term b => b -> b) -> a -> a
  gmapT phi = runIdentity . gfoldl phi' Identity
    where phi' (Identity i) x = Identity (i $ phi x)

  gmapQ :: (forall b. Term b => b -> r) -> a -> [r]
  gmapQ phi = reverse . getConst . gfoldl phi' (const (Const []))
    where phi' (Const rs) x = Const $ phi x : rs

  gmapM :: Monad m => (forall b. Term b => b -> m b) -> a -> m a
  gmapM phi = gfoldl phi' return
    where phi' mf x =
            do f <- mf
               x' <- phi x
               return $ f x
  
  gfoldl 
    :: (forall x y. Term x => w (x -> y) -> x -> w y)
    -> (forall z. z -> w z)
    -> a -> w a

--------------------------------------------------------------------------------

instance Typeable a => Typeable (IO a) where
  typeOf u = TyRep "Prelude.IO" [typeOf $ phi u]
    where phi :: IO a -> a
          phi = undefined

instance Typeable Bool where
  typeOf _ = TyRep "Prelude.Bool" []
instance Term Bool where
  -- gmapT f x = x
  -- gmapQ f x = []
  -- gmapM f = return
  gfoldl theta r = r

instance Typeable Int where
  typeOf _ = TyRep "Prelude.Int" []
instance Term Int where
  -- gmapT f x = x
  -- gmapQ f x = []
  -- gmapM f = return
  gfoldl theta r = r

instance Typeable Char where
  typeOf _ = TyRep "Prelude.Char" []
instance Term Char where
  -- gmapT f x = x
  -- gmapQ f x = []
  -- gmapM f = return
  gfoldl theta r = r


instance (Typeable a, Typeable b) => Typeable (a -> b) where
  typeOf f = TyRep "Prelude.->" [typeOf $ phi f, typeOf $ theta f]
    where phi :: (a -> b) -> a
          phi = undefined
          theta :: (a -> b) -> b
          theta = undefined


instance Typeable a => Typeable [a] where
  typeOf xs = TyRep "Prelude.List" [typeOf $ phi xs]
    where phi :: [a] -> a
          phi = undefined
instance Term a => Term [a] where
  -- gmapT f [] = []
  -- gmapT f (a:as) = f a : f as

  -- gmapQ f [] = []
  -- gmapQ f (a:as) = [f a, f as]

  -- gmapM f [] = return []
  -- gmapM f (a:as) = (:) <$> f a <*> f as

  gfoldl k r [] = r []
  gfoldl k r (a:as) = r (:) `k` a `k` as

instance (Typeable a, Typeable b) => Typeable (a, b) where
  typeOf p = TyRep "Prelude.(,)" [typeOf $ phi p, typeOf $ theta p]
    where phi :: (a, b) -> a
          phi = undefined
          theta :: (a, b) -> b
          theta = undefined
instance (Term a, Term b) => Term (a, b) where
  -- gmapT f (a,b) = (f a, f b)

  -- gmapQ f (a,b) = [f a, f b]

  -- gmapM f (a,b) = (,) <$> f a <*> f b

  gfoldl k r (a,b) = r (,) `k` a `k` b


instance (Typeable a, Typeable b) => Typeable (Either a b) where
  typeOf e = TyRep "Prelude.Either" [typeOf $ phi e, typeOf $ theta e]
    where phi :: Either a b -> a
          phi = undefined
          theta :: Either a b -> b
          theta = undefined
instance (Term a, Term b) => Term (Either a b) where
  -- gmapT f (Left a) = Left $ f a
  -- gmapT f (Right b) = Right $ f b

  -- gmapQ f (Left a) = [f a]
  -- gmapQ f (Right b) = [f b]

  -- gmapM f (Left a) = Left <$> f a
  -- gmapM f (Right b) = Right <$> f b

  gfoldl k r (Left a) = r Left `k` a
  gfoldl k r (Right b) = r Right `k` b

instance Typeable a => Typeable (Maybe a) where
  typeOf e = TyRep "Prelude.Maybe" [typeOf $ phi e]
    where phi :: Maybe a -> a
          phi = undefined
instance Term a => Term (Maybe a) where
  -- gmapT f Nothing = Nothing
  -- gmapT f (Just a) = Just (f a)

  -- gmapQ f Nothing = []
  -- gmapQ f (Just a) = [f a]

  -- gmapM f Nothing = return Nothing
  -- gmapM f (Just b) = Just <$> f b

  gfoldl k r Nothing = r Nothing
  gfoldl k r (Just a) = r Just `k` a


cast :: (Typeable a, Typeable b) => a -> Maybe b
cast a = r
  where r = if typeOf (Just a) == typeOf r
              then Just (unsafeCoerce a)
              else Nothing

mkT :: (Typeable a, Typeable b)
  => (a -> a) -> b -> b
mkT f = case cast f of
  Nothing -> id
  Just g -> g

exTwo :: (Typeable a, Typeable b, Typeable c) 
  => (a -> a) -> (b -> b) -> c -> c
exTwo f g = 
  case cast f of
    Just f' -> f'
    Nothing ->
      case cast g of
        Just g' -> g'
        Nothing -> id

ex :: Typeable c 
  => (forall a. Typeable a => a -> a)
  -> (forall b. Typeable b => b -> b)
  -> c -> c
ex f g c = g (f c)

everywhere 
  :: Term a 
  => (forall b. Typeable b => b -> b)
  -> a -> a
everywhere phi a = phi $ gmapT (everywhere phi) a



mkQ :: (Term a, Term b) => r -> (a -> r) -> b -> r
mkQ r f b = case cast b of
  Just a -> f a
  Nothing -> r

everything :: Term a => (r -> r -> r) -> (forall b. Term b => b -> r) -> a -> r
everything f q a = foldl f (q a) (gmapQ (everything f q) a)

mkM :: (Monad m, Typeable (m a), Typeable (m b), Term a, Term b) 
  => (a -> m a) -> b -> m b
mkM f
  = case cast f of
      Just g -> g
      Nothing -> return

everywhereM :: (Monad m, Term a) => (forall b. Term b => b -> m b) -> a -> m a
everywhereM phi a =
  do a' <- gmapM (everywhereM phi) a
     phi a'

