
import Data.Vect

max : Nat -> Nat -> Nat
max (S a) (S b) = S (max a b)
max Z b = b
max a Z = a

min : Nat -> Nat -> Nat
min (S k) (S j) = ?min_rhs_5
min Z (S k) = ?min_rhs_4
min (S k) Z = ?min_rhs_1


even : Integer -> Bool
even x = (x `mod` 2) == 0


data DepPair : (a : Type) -> (T : a -> Type) -> Type where
  MkDepPair : {T : a -> Type} -> (x : a) -> T x -> DepPair a T

data MyPair : (a : Type) -> (b : Type) -> Type where
  MkPair : a -> b -> MyPair a b

filt : (a -> Bool) -> Vect n a -> DepPair Nat (\k => Vect k a)
filt _ [] = MkDepPair _ []
filt pred (a::as) 
  = let MkDepPair m as' = filt pred as
     in if pred a 
           then MkDepPair (S m) (a :: as')
           else MkDepPair m as'


data Parity : Nat -> Type where
  Even : Parity (n + n)
  Odd : Parity (S (n + n))

-- parity : (n : Nat) -> Parity n
-- parity Z = Even {n=Z}
-- parity (S Z) = Odd {n=Z}
-- parity (S (S n)) = ?rhs

myLen : {n : Nat} -> Vect n a -> Nat
myLen [] = 0
myLen (a::as) = 1 + myLen as

data FinSet : Nat -> Type where
  FZero : FinSet (S k)
  FSucc : FinSet k -> FinSet (S k)

snoc : Vect n a -> a -> Vect (S n) a
snoc xs x = ?snoc_rhs -- xs ++ [x]

reve : Vect n a -> Vect n a
reve [] = []
reve (a::as) = ?rhsreve
  -- reve as ++ [a]

dup : Vect n a -> Vect (n + n) a
dup [] = []
dup (x :: xs) = ?dup_rhs -- x :: ((x :: dup xs))


-- data MultiList : List Type -> Type where
--   Nil : forall a. MultList a

Matrix : Nat -> Nat -> Type -> Type
Matrix m n a = Vect m (Vect n a)

transpose : Matrix m n a -> Matrix n m a
transpose [] = []
transpose (xs::xxs) = zip (:) xs (transpose xxs)
