{-# LANGUAGE Rank2Types #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.List


class MonadPlus m => LogicM m where
  split :: m a -> m (Maybe (a, m a))
  -- split mzero === return Nothing
  -- split (return a `mplus` v) === return (Just (a, v))

  interleave :: m a -> m a -> m a
  interleave u v = 
    do u_ <- split u
       case u_ of
         Nothing -> v
         -- interleave mzero v === v
         Just (a, u') ->
           return a `mplus` interleave v u'
         -- interleave (return a `mplus` u) v === return a `mplus` interleave v u

  (>>-) :: m a -> (a -> m b) -> m b
  u >>- f =
    do u_ <- split u
       case u_ of
         Nothing -> mzero
         -- mzero >>- f === mzero
         Just (a, u') ->
           interleave (f a) (u' >>- f)
         -- (return a `mplus` u) >>- f === f a `interleave` (u >>- f)

  once :: m a -> m a
  once u = 
    do u_ <- split u
       case u_ of
         Just (a, _) -> return a
         -- once (return a `mplus` u) = return a
         Nothing -> mzero
         -- once mzero = mzero

  ifte :: m a -> (a -> m b) -> m b -> m b
  ifte b th el =
    do b_ <- split b
       case b_ of
         Nothing -> el
         -- ifte mzero _ el === el
         Just (a, u') ->
           th a `mplus` (u' >>= th)
         -- ifte (return a `mplus` u) th _ === a th `mplus` u >>= th
           -- why not this though? 
           -- interleave (th a) (u' >>- th)

newtype Logic a 
  = Logic { runLogic :: forall r. (a -> r -> r) -> r -> r }

allOf :: Logic a -> [a]
allOf (Logic phi)
  -- = phi (\a r -> a:r) []
  = phi (:) []

backtrackWith :: [a] -> Logic a
backtrackWith = foldr (\a r -> pure a <|> r) empty

oneOf :: Logic a -> Maybe a
oneOf (Logic phi)
  = phi (\a _ -> Just a) Nothing

instance Functor Logic where
  fmap f (Logic u) 
    = Logic $ \suc fa -> u (\a r -> suc (f a) r) fa
  
instance Applicative Logic where
  pure a = Logic $ \suc fa -> suc a fa
  Logic uf <*> Logic v
    = undefined
    -- = Logic $ \suc fa -> 

instance Monad Logic where
  return = pure
  Logic u >>= f 
    -- = undefined
    = Logic $ \suc fa
        -> u (\a r -> runLogic (f a) suc r) fa
        -- suc (\a r -> runLogic (f a) suc fa) fa

instance Alternative Logic where
  empty = Logic $ \_ r -> r
  (Logic u) <|> (Logic v)
    = Logic $ \suc fa -> u suc (v suc fa)

instance MonadPlus Logic where

reflect :: Maybe (a, Logic a) -> Logic a
reflect Nothing = mzero
reflect (Just (a,l)) = pure a <|> l

instance LogicM Logic where
  split (Logic u) 
    = u phi (return Nothing)
    where phi a log = return $ Just (a, log >>= reflect)

--------------------------------------------------------------------------------

instance LogicM [] where
  split [] = [Nothing]
  split (a:as) = [Just (a, as)]

vfefe :: String -> Logic String
vfefe i
  | (lcon,(lvow,rest)) <- span isVowel <$> break isVowel i
    , [ncon, swapped, nvow, swapped', nvow'] <- rest
    , ncon == swapVoice swapped && swapped == swapped' && nvow == nvow'
    = ((lcon ++ lvow ++ [ncon]) ++) <$> firstVowel nvow
    -- [ncon, swapVoiceMap swapped, nvow]  ]
    --
  | otherwise = empty

firstVowel :: Char -> Logic String
firstVowel v 
  = pow consonants 
  >>- \cons -> ((cons ++ [v]) ++) <$> pow alphabet

consonants :: Logic Char
consonants = backtrackWith "bcdfghjklmnpqrstvwxz"

alphabet :: Logic Char
alphabet = backtrackWith "abcdefghijklmnopqrstuvwxyz"

--------------------------------------------------------------------------------

pow :: Logic a -> Logic [a]
pow as = -- [0..] >>= powN as
  pure [] <|> (pow as >>= (\aas -> fmap (:aas) as))

powN :: [a] -> Int -> [[a]]
powN _ 0 = [[]]
powN as n = 
  do aas <- powN as (n-1)
     fmap (:aas) as

--------------------------------------------------------------------------------

covfefe :: String -> String
covfefe i = unwords $ map covfefify (words i)

isVowel :: Char -> Bool
isVowel h = h `elem` ("aeiouy" :: String)

swapVoice :: Char -> Char
swapVoice c = "pgt.vkh.jglmn.bqrzd.fwx.s" !! (fromEnum c-98) 

-- https://codegolf.stackexchange.com/questions/123685/covfefify-a-string
covfefify :: String -> String
covfefify i
  | (lcon,(lvow,ncon:rest)) <- span isVowel <$> break isVowel i
    , nvow:_ <- filter isVowel rest
    , swap <- swapVoice ncon 
    = lcon ++ lvow ++ [ncon, swap, nvow, swap, nvow]
  | otherwise = i
