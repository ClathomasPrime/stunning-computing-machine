
data Natural : Type where
  Zero : Natural
  Succ : Natural -> Natural

add : Natural -> Natural -> Natural
add Zero n = n
add (Succ a) n = Succ (add a n)

data Vect : Natural -> Type -> Type where
  Nil : Vect Zero a
  Cons : a -> Vect n a -> Vect (Succ n) a

zipUp : Vect n a -> Vect n b -> Vect n (a,b)
zipUp Nil Nil = Nil
zipUp (Cons a as) (Cons b bs) = Cons (a,b) (zipUp as bs)

head : Vect (Succ n) a -> a
head (Cons a _) = a

unsafeHead : Vect n a -> a
unsafeHead (Cons a _) = a
