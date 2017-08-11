
class Category arr where
  identity :: arr a a
  (>>>) :: arr a b -> arr b c -> arr a c

class Category arr => Arrow arr where
  arr :: (x -> y) -> arr x y
  first :: arr x y -> arr (x,b) (y,b)
  second :: arr x y -> arr (a,x) (a,y)

instance Category (->) where
  identity = id
  (>>>) = flip (.)

instance Arrow (->) where
  arr = id
  first f (x,b) = (f x, b)
  second f (a,x) = (a,f x)

split :: Arrow arr => arr x (x,x)
split = arr (\x -> (x,x))

(***) :: Arrow arr => arr a x -> arr b y -> arr (a,b) (x,y)
f *** g = first f >>> second g

