module UDPatterns where

import UDConcepts
import GFConcepts
import UDAnalysis


showMatchesInUDSentence :: UDPattern -> UDSentence -> String
showMatchesInUDSentence p s =
  if   null matches then ""
  else unlines (udCommentLines s ++ matches)
 where
   matches = [prt (udTree2sentence t) | t <- matchesUDPattern p (udSentence2tree s)]

matchesUDPattern :: UDPattern -> UDTree -> [UDTree]
matchesUDPattern p tree@(RTree node subtrees) =
  [tree | ifMatchUDPattern p tree] ++
  concatMap (matchesUDPattern p) subtrees

showReplacementsInUDSentence :: UDReplacement -> UDSentence -> String
showReplacementsInUDSentence rep s =
  prt (ns{
    udCommentLines = udCommentLines s ++ ["# newtext = " ++ unwords (map udFORM (udWordLines ns))]
    })
 where
   ns = adjustUDIds (udTree2sentence (replacementsWithUDPattern rep (udSentence2tree s)))

replacementsWithUDPattern :: UDReplacement -> UDTree -> UDTree
replacementsWithUDPattern rep tree = case replaceWithUDPattern rep tree of
  RTree node subtrs -> RTree node (map (replacementsWithUDPattern rep) subtrs)

data UDPattern =
    FORM String
  | LEMMA String
  | POS String
  | FEATS UDDatas  -- feature list matches exactly
  | FEATS_ UDDatas -- a sublist of features matches exactly
  | DEPREL String   -- deprel matches exactly
  | DEPREL_ String -- prefix part of deprel matches, e.g. nsubj:pass matches nsubs
  | AND UDPattern UDPattern
  | OR UDPattern UDPattern
  | NOT UDPattern
  | TREE UDPattern [UDPattern] -- subtrees match exactly
  | TREE_ UDPattern [UDPattern] -- some sublist of subtrees matches exactly
  | TRUE
  | ARG String String
  | DEPTH_EQUALS Int
  | DEPTH_UNDER Int
  | DEPTH_OVER Int
 deriving (Show,Read)

ifMatchUDPattern :: UDPattern -> UDTree -> Bool
ifMatchUDPattern patt tree@(RTree node subtrees) = case patt of
  FORM s -> udFORM node == s
  LEMMA s -> udLEMMA node == s
  POS s -> udUPOS node == s
  FEATS udds -> udFEATS node == uddatas2list udds
  FEATS_ udds ->
    let uddlist = uddatas2list udds in
    or [fs == uddlist | fs <- sublists (length uddlist) (udFEATS node)]
  DEPREL s -> udDEPREL node == s
  DEPREL_ s -> takeWhile (/=':') (udDEPREL node) == s
  AND p q -> ifMatchUDPattern p tree && ifMatchUDPattern q tree
  OR p q -> ifMatchUDPattern p tree || ifMatchUDPattern q tree
  NOT p -> not (ifMatchUDPattern p tree)
  TREE p ps -> ifMatchUDPattern p tree
    && length ps == length subtrees
    && and [ifMatchUDPattern q t | (q,t) <- zip ps subtrees]
  TREE_ p ps ->
    or [ifMatchUDPattern (TREE p ps) (RTree node qs) | qs <- sublists (length ps) subtrees]
  TRUE -> True
  ARG pos deprel -> ifMatchUDPattern (AND (POS pos) (DEPREL deprel)) tree
  DEPTH_EQUALS d -> depthRTree tree == d
  DEPTH_UNDER d -> depthRTree tree < d
  DEPTH_OVER d -> depthRTree tree > d


data UDReplacement =
    REPLACE UDPattern UDPattern
  | PRUNE UDPattern  -- drop dependents, shorthand for FLATTEN p 0
  | REMOVE UDPattern -- drop the whole subtree, if not the root
  | FLATTEN UDPattern Int -- cut the tree at depth Int
 deriving (Show,Read)

replaceWithUDPattern :: UDReplacement -> UDTree -> UDTree
replaceWithUDPattern rep tree@(RTree node subtrs) = case rep of
  REPLACE cond change | ifMatchUDPattern cond tree -> case change of
    FORM s -> tree{root = node{udFORM = s}}
    LEMMA s -> tree{root = node{udLEMMA = s}}
    POS s -> tree{root = node{udUPOS = s}}
    DEPREL s -> tree{root = node{udDEPREL = s}}
  PRUNE cond | ifMatchUDPattern cond tree -> tree{subtrees = []}
  REMOVE cond -> RTree node [st | st <- subtrs, not (ifMatchUDPattern cond st)]
  FLATTEN cond depth | ifMatchUDPattern cond tree -> flattenRTree depth tree
  _ -> tree

flattenRTree :: Int -> RTree a -> RTree a
flattenRTree d tr@(RTree node subtrs) = case d of
  0 -> RTree node []
  _ -> RTree node (map (flattenRTree (d-1)) subtrs)

--------------------------------------------------
--- a hack to read FEATS with their usual syntax
data UDDatas =
    NIL
  | CONS UDData UDDatas
  deriving Show ----

uddatas2list :: UDDatas -> [UDData]
uddatas2list l = case l of
  NIL -> []
  CONS d ds -> d : uddatas2list ds

list2uddatas :: [UDData] -> UDDatas 
list2uddatas l = case l of
  [] -> NIL
  d:ds -> CONS d (list2uddatas ds)

instance Read UDDatas where
  readsPrec _ s = [(list2uddatas (prs s),"")]
-------------------------------------------------

-- convenience, must be in some standard lib...

sublists :: Int -> [a] -> [[a]]
sublists n xs = case (n,xs) of
  (0,_)  -> [[]]
  (_,[]) -> []
  (_,x:xx) -> [x:ys | ys <- sublists (n-1) xx] ++ sublists n xx



 
