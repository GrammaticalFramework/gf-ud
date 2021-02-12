module UDPatterns where

import UDConcepts
import GFConcepts
import UDAnalysis


showMatchesInUDSentence :: UDPattern -> UDSentence -> String
showMatchesInUDSentence p s =
  if   null matches then ""
  else unlines (udCommentLines s ++ matches)
 where
   matches = [prUDTree t | t <- matchesUDPattern p (udSentence2tree s)]

matchesUDPattern :: UDPattern -> UDTree -> [UDTree]
matchesUDPattern p tree@(RTree node subtrees) =
  [tree | ifMatchUDPattern p tree] ++
  concatMap (matchesUDPattern p) subtrees

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



 
