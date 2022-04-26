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
-- mapRTree f (RTree c ts) = RTree (f c) (map (mapRTree f) ts)

allNodesRTree :: RTree a -> [a]
allNodesRTree = toList
-- allNodesRTree t = root t : concatMap allNodesRTree (subtrees t)

-- Should be equivalent to foldr from the derived Foldable instance
-- foldrRTree :: (a -> b -> b) -> b -> RTree a -> b
-- foldrRTree f z t = f (root t) (foldr (flip (foldrRTree f)) z (subtrees t))

-- Not sure if this is useful for anything
-- cataRTree :: (a -> [b] -> b) -> RTree a -> b
-- cataRTree f t = f (root t) (map (cataRTree f) (subtrees t))

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

{-
Generated instance, shown with {-# OPTIONS_GHC -ddump-deriv  #-}

instance Data.Foldable.Foldable RTree.RTree where
    Data.Foldable.foldr f z (RTree.RTree a1 a2)
      = f a1
          ((\ b3 b4
              -> Data.Foldable.foldr
                   (\ b1 b2 -> Data.Foldable.foldr f b2 b1) b4 b3)
             a2 z)
    Data.Foldable.foldMap f (RTree.RTree a1 a2)
      = GHC.Base.mappend
          (f a1) (Data.Foldable.foldMap (Data.Foldable.foldMap f) a2)
    Data.Foldable.null (RTree.RTree _ _) = GHC.Types.False
  

-}