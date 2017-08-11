{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}


class Functor g => Representable g where
  type Key g :: *

  tabulate :: (Key g -> a) -> g a

  index :: g a -> Key g -> a

distributeRep :: (Functor f, Representable g)
  => f (g a) -> g (f a)
distributeRep = tabulate . distributeReader . fmap index
--    g (f a) <-- (Key g -> f a) <-- f (Key g -> a) <-- f (g a)

distributeReader :: Functor f => f (r -> a) -> r -> f a
distributeReader u r = fmap ($r) u

class Functor g => Distribute g where
  distribute :: Functor f => f (g a) -> g (f a)
