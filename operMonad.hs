{-# LANGUAGE GADTs, TypeFamilies #-}

import System.Random

-- data H

-- data Encripted a where
--   Encript :: Encriptable a => a -> Encripted a
--   Decript :: Encriptabe
  
data StackInstr a where
  Push :: Int -> StackInstr ()
  Pop :: StackInstr Int

data Program instr a where
  Then :: instr b -> (b -> Program instr a) -> Program instr a
  Return :: a -> Program instr a

unit :: instr a -> Program instr a
unit instr = instr `Then` Return

instance Functor (Program instr) where
  fmap f (Return a) = Return (f a)
  fmap f (instr `Then` rest) = instr `Then` (fmap f . rest)

instance Applicative (Program instr) where
  pure = Return
  Return f <*> u = fmap f u
  (instr `Then` rest) <*> u = instr `Then` (\b -> rest b <*> u)

instance Monad (Program instr) where
  Return a >>= phi = phi a
  (instr `Then` rest) >>= phi = instr `Then` \b -> rest b >>= phi


interpretStack :: Program StackInstr a -> [Int] -> a
interpretStack (Return a) _ = a
interpretStack (Push i `Then` phi) is = interpretStack (phi ()) (i:is)
interpretStack (Pop `Then` phi) (i:is) = interpretStack (phi i) is
interpretStack (Pop `Then` _) [] = error "poped an empty stack"
  
example :: Program StackInstr Int
example =
  Push 3 `Then` \_ ->
  Push 4 `Then` \_ ->
  Pop `Then` \a ->
  Pop `Then` \b ->
  Return (a+b)


class Instruction instr where
  type State instr :: *

  step :: State instr -> instr a -> (a, State instr)

interpret :: Instruction instr => State instr -> Program instr a -> a
interpret _ (Return a) = a
interpret st (instr `Then` rest) 
  = let (b, st') = step st instr
     in interpret st' (rest b)


instance Instruction StackInstr where
  type State StackInstr = [Int]

  step ns (Push n) = ((), n:ns)
  step (n:ns) Pop = (n, ns)
  step [] Pop = error "oops popped empty"


data RandomInstr a where
  Uniform :: [a] -> RandomInstr a
  -- RandomInt :: RandomInstr Int

interpretStdGen :: StdGen -> Program RandomInstr a -> a
interpretStdGen gen (Return a) = a
interpretStdGen gen (Uniform bs `Then` rest) 
  = let (i, gen') = next gen
        i' = i `mod` length bs
     in interpretStdGen gen' $ rest (bs !! i')

interpretDistr :: Program RandomInstr a -> [(a,Double)]
interpretDistr (Return a) = [(a, 1.0)]
interpretDistr (Uniform bs `Then` rest)
  = do b <- bs
       (a,pr) <- interpretDistr (rest b)
       return (a, pr / fromIntegral (length bs))

-- instance Instruction RandomInstr where
--   type State RandomInstr = StdGen
-- 
--   step gen (Uniform as) =
--     let (i, gen') = next gen
--         i' = i `mod` length as
--      in (as !! i', gen')
--   step gen (Uniform as) =

rexample :: Program RandomInstr String
rexample = 
  do x <- unit $ Uniform "aeiou"
     y <- unit $ Uniform "bdgj"
     return $ x:y:[]


