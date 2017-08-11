{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding(Functor, Monad, join, (>>=), pure, fmap, return)

class CoMonoid w where
  comempty :: w -> ()
  comappend :: w -> (w,w)

instance CoMonoid w where
  comempty = const ()
  comappend w = (w,w)

class MyFunc f where
  fmap :: (a -> b) -> f a -> f b

class MyFunc m => Mynad m where
  join :: m (m a) -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  pure :: a -> m a

  -- fmap f = (>>= pure . f)
  join = (>>= id)
  u >>= f = join . fmap f $ u

class MyFunc w => CoMynad w where
  extract :: w a -> a
  (=>>) :: w a -> (w a -> b) -> w b
  duplicate :: w a -> w (w a)

  -- fmap f = (=>> f . extract)
  duplicate = (=>> id)
  u =>> f = fmap f . duplicate $ u

instance MyFunc ((,) w) where
  fmap f (w,a) = (w,f a)

instance MyFunc ((->) r) where
  fmap f rw = f . rw

instance Monoid w => Mynad ((,) w) where
  pure = (,) mempty
  join (w, (w',a)) = (w `mappend` w, a)
  (w,a) >>= f 
    = let (w',b) = f a
       in (w`mappend`w', b)

instance CoMonoid r => Mynad ((->) r) where
  pure = const
  join phi r = phi r r
  u >>= f 
    = \r -> f (u r) r

instance CoMonoid w => CoMynad ((,) w) where
  extract (_,a) = a
  duplicate (w,a) = (w,(w,a))
  (w,a) =>> f = (w, f (w,a))

instance Monoid r => CoMynad ((->) r) where
  extract u = u mempty
  duplicate u r r' = u (r `mappend` r')
  -- u =>> f = ^





-- instance Co
