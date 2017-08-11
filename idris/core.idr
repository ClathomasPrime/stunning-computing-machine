module Main



revv' : List a -> List a -> List a
revv' Nil xs = xs
revv' (a::as) xs = revv' as (a::xs)

revv : List a -> List a
revv as = revv' as Nil

data Natural : Type where
  Zero : Natural
  Succ : Natural -> Natural

Plus : Natural -> Natural -> Natural
Plus Zero x = x
Plus (Succ n) x = Succ (Plus x n)

-- total zPlusN : (m : Natural) -> Plus Zero m = m
-- zPlusN right = Refl

data Vect : (n : Natural) -> (a : Type) -> Type where
  Nil : Vect Zero a
  Cons : a -> Vect n a -> Vect (Succ n) a

-- length : Vect n a -> Natural
-- length (xs : Vect n a) = n

-- app : Vect n a -> Vect m a -> Vect (Plus n m) a
-- app Nil xs = ?rhs_1
-- app (Cons a as) xs = ?rhs_2 -- Cons a (app as xs)


