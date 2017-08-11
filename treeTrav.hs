{-# LANGUAGE MultiParamTypeClasses #-} 
import Data.List

data Tree a
  = Branch a (Tree a) (Tree a)
  | Nil
  deriving(Show)

buildTree :: Eq a => [a] -> [a] -> Tree a
buildTree [] _ = Nil
buildTree (pr:prs) infs
  = let (lIns, _:rIns) = span (/=pr) infs
        (lPres, rPres) = splitAt (length lIns) prs
        lft = buildTree lPres lIns
        rgt = buildTree rPres rIns
     in Branch pr lft rgt

main = print "hi"


levelOrder :: Tree a -> [a]
levelOrder = concat . levelOrder'

levelOrder' :: Tree a -> [[a]]
levelOrder' Nil = []
levelOrder' (Branch a l r)
  = [a] : phi (levelOrder' l) (levelOrder' r)
  where phi :: [[a]] -> [[a]] -> [[a]]
        phi [] r = r
        phi l [] = l
        phi (l:ls) (r:rs) = (l ++ r) : phi ls rs
