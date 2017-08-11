
data FunList a b
  = Done b
  | More a (FunList a (a -> b))

getAs :: FunList a b -> [a]
getAs (Done _) = []
getAs (More a u) = a : getAs u

getB :: FunList a b -> b
getB (Done b) = b
getB (More a u) = (getB u) a

instance Functor (FunList a) where
  fmap f (Done b) = Done (f b)
  fmap f (More a u) = More a (fmap (f .) u)

instance Applicative (FunList a) where
  pure = Done
  Done f <*> v = fmap f v
  More a u <*> v = More a (flip <$> u <*> v)
    -- (\phi -> (\x -> phi a x)) <$> u <*> v

zipFun :: FunList a x -> FunList b y -> FunList (a,b) (x,y)
zipFun (More a u) (More b v) = More (a,b) $ fmap factor (zipFun u v)
zipFun (Done x) (Done y) = Done (x,y)

factor :: (a -> x, b -> y) -> (a,b) -> (x,y)
factor (phi, theta) (a,b) = (phi a, theta b)

-- bimap :: (a -> x) -> (b -> y) -> (a,b) -> (x,y)
-- bimap f g (a,b) = (f a, g b)



hotStuff :: FunList Int (String -> (String, Int))
hotStuff = More 9 (More 11 $ Done $ \a b -> \s -> (reverse s, a + b + length s))

goodFun :: FunList Int String
goodFun = More 1 (More 5 (More 6 $ Done $ \a b c -> show (a + b + c)))

redShirt :: FunList Char String
redShirt = More 'a' (More 'b' (More 'c' $ Done $ \a b c -> a:b:c:[]))

