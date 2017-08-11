{-# LANGUAGE Rank2Types #-}

newtype Sum a b 
  = Sum { runSum :: forall c. (a -> c) -> (b -> c) -> c }

alpha :: Sum a b -> Either a b
alpha (Sum s) = s Left Right

beta :: Either a b -> Sum a b
beta (Left a) = Sum $ \f _ -> f a
beta (Right b) = Sum $ \_ g -> g b

newtype Product a b 
  = Product { runProduct :: forall c. (a -> b -> c) -> c }

phi :: Product a b -> (a,b)
phi (Product p) = p (,)

theta :: (a,b) -> Product a b
theta (a,b) = Product $ \f -> f a b
