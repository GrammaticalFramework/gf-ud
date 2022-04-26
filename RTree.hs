{-# LANGUAGE DeriveTraversable #-}
module RTree where
import Data.Foldable (toList)

-- rose tree
data RTree a = RTree {
  root   :: a,
  subtrees :: [RTree a]
  }
  deriving (Eq,Show,Read, Functor, Foldable)

mapRTree :: (a -> b) -> RTree a -> RTree b
mapRTree = fmap

allNodesRTree :: RTree a -> [a]
allNodesRTree = toList

prLinesRTree :: (a -> String) -> RTree a -> String
prLinesRTree prt = unlines . pr 0 where
  pr i t = indent i (prt (root t)) : concatMap (pr (i+4)) (subtrees t)
  indent i s = replicate i ' ' ++ s

prRTree :: (a -> String) -> RTree a -> String
prRTree pr t = case t of
  RTree a [] -> pr a
  RTree a ts -> "(" ++ pr a ++ " " ++ unwords (map (prRTree pr) ts) ++ ")"

isSubRTree :: Eq a => RTree a -> RTree a -> Bool
isSubRTree t u = t == u || any (isSubRTree t) (subtrees u)

sizeRTree :: RTree a -> Int
sizeRTree = length
-- sizeRTree = length . allNodesRTree

depthRTree :: RTree a -> Int
depthRTree (RTree _ []) = 1
depthRTree (RTree _ ts) = 1 + maximum (map depthRTree ts)

leavesRTree :: RTree a -> [a]
leavesRTree t = case t of
  RTree a [] -> [a]
  RTree a ts -> concatMap leavesRTree ts

childrenRTree :: RTree a -> [RTree a]
childrenRTree (RTree _ ts) = ts

allSubRTrees :: RTree a -> [RTree a]
allSubRTrees t = t : concatMap allSubRTrees (childrenRTree t)