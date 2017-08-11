{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


class Functor f => App f where
  zoot :: f ()

  scoot :: f a -> f b -> f (a, b)

instance (Functor f, Applicative f) => App f where
  zoot = pure ()

  scoot u v = (,) <$> u <*> v

instance (Functor f, App f) => Applicative f where
  pure x = const x <$> zoot

  u <*> v = uncurry ($) <$> (u `scoot` v)
