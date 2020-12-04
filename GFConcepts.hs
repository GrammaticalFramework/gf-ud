module GFConcepts where

import PGF

import Data.List



-- standard GF abstract tree built from constructors
type AbsTree = RTree Fun

type Cat = CId
type Fun = CId

type AbsType = (Cat,[Cat])

prAbsType (val,args) = unwords $ intersperse "->" $ map showCId $ args ++ [val]
prAbsTree = showExpr [] . abstree2expr

pAbsTree s = case readExpr s of
  Just e -> expr2abstree e
  _ -> error $ "cannot parse abstree " ++ s

pAbsType s = case (filter (/="->") (words s)) of
  cs@(_:_) -> (mkCId (last cs), map mkCId (init cs))
  _ -> error $ "cannot parse abstype " ++ s


pgf2functions :: PGF -> [(Fun,AbsType)]
pgf2functions pgf = [(fun,(val,[arg | (_,_,ty) <- hs, let (_,arg,_) = unType ty])) |
  cat <- categories pgf,
  fun <- functionsByCat pgf cat,
  Just typ <- [functionType pgf fun],
  let (hs,val,_) = unType typ
  ]

-- conversion from PGF to rose tree

expr2abstree :: PGF.Expr -> AbsTree
expr2abstree e = case unApp e of
  Just (f,es) -> RTree f (map expr2abstree es)
  _ -> error ("ERROR: no constructor tree from " ++ showExpr [] e)

abstree2expr :: AbsTree -> PGF.Expr
abstree2expr tr@(RTree f ts) = mkApp f (map abstree2expr ts)

postOrderRTree :: RTree a -> RTree (a,Int)
postOrderRTree = post 0 where

  post :: Int -> RTree a -> RTree (a,Int)
  post n t = case t of
    RTree a ts -> case posts n ts of
      (nts,nn) -> RTree (a,nn) nts
      
  posts :: Int -> [RTree a] -> ([RTree (a,Int)],Int)
  posts n ts = case ts of
    []   -> ([],n)
    t:tt -> case post n t of
      nt@(RTree (_,nn) _) -> case posts (nn+1) tt of
        (nts,nnn) -> (nt:nts,nnn)

-- rose tree
data RTree a = RTree {
  root   :: a,
  subtrees :: [RTree a] 
  }
 deriving (Eq,Show)

mapRTree :: (a -> b) -> RTree a -> RTree b
mapRTree f (RTree c ts) = RTree (f c) (map (mapRTree f) ts)

allNodesRTree :: RTree a -> [a]
allNodesRTree t = root t : concatMap allNodesRTree (subtrees t)

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
sizeRTree = length . allNodesRTree

leavesRTree :: RTree a -> [a]
leavesRTree t = case t of
  RTree a [] -> [a]
  RTree a ts -> concatMap leavesRTree ts

-----------------

mkFun :: [String] -> CId -> CId
mkFun ws c = mkCId $ concat $ intersperse "_" (ws ++ [showCId c])

partsOfFun :: CId -> [String]
partsOfFun f = words (map (\c -> if c=='_' then ' ' else c) (showCId f))

partsOfFileName :: FilePath -> (String,String,String,String)
partsOfFileName s = (path,abstr,lang,ext)
  where
    (path,file) = case break (=='/') s of
      (p,_:f) -> (p,f)
      _ -> ("",s)
    (modul,_:ext) = break (=='.') file
    (abstr,lang) = splitAt (length modul - 3) modul



