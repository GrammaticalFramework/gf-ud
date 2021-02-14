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
matchesUDPattern p tree@(RTree node subtrees) = case p of
  SEQUENCE ps  -> maybe [] return $ findMatchingUDSequence True ps tree
  SEQUENCE_ ps  -> maybe [] return $ findMatchingUDSequence False ps tree
  _ -> [tree | ifMatchUDPattern p tree] ++ concatMap (matchesUDPattern p) subtrees

showReplacementsInUDSentence :: UDReplacement -> UDSentence -> String
showReplacementsInUDSentence rep s =
  prt (ns{
    udCommentLines = udCommentLines s ++
    if changed then ["# newtext = " ++ unwords (map udFORM (udWordLines ns))] else []
    })
 where
   ns = adjustUDIds (udTree2sentence tr)
   (tr,changed) = replacementsWithUDPattern rep (udSentence2tree s)

replacementsWithUDPattern :: UDReplacement -> UDTree -> (UDTree,Bool)
replacementsWithUDPattern rep tree = case replaceWithUDPattern rep tree of
  (RTree node subtrs,b) -> let (trs,bs) = unzip (map (replacementsWithUDPattern rep) subtrs)
                           in (RTree node trs, or (b:bs))

data UDPattern =
    FORM String
  | LEMMA String
  | POS String
  | FEATS UDDatas  -- feature list matches exactly
  | FEATS_ UDDatas -- a sublist of features matches exactly
  | DEPREL String   -- deprel matches exactly
  | DEPREL_ String -- prefix part of deprel matches, e.g. nsubj:pass matches nsubs
  | AND [UDPattern]
  | OR [UDPattern]
  | NOT UDPattern
  | SEQUENCE [UDPattern]  -- the smallest subtree where patterns appear in linear sequence
  | SEQUENCE_ [UDPattern]  -- as SEQUENCE, but holes between words are permitted
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
  AND ps -> and [ifMatchUDPattern p tree | p <- ps]
  OR ps -> or [ifMatchUDPattern p tree | p <- ps]
  NOT p -> not (ifMatchUDPattern p tree)
  SEQUENCE ps -> maybe False (const True) $ findMatchingUDSequence True ps tree
  SEQUENCE_ ps -> maybe False (const True) $ findMatchingUDSequence False ps tree
  TREE p ps -> ifMatchUDPattern p tree
    && length ps == length subtrees
    && and [ifMatchUDPattern q t | (q,t) <- zip ps subtrees]
  TREE_ p ps ->
    or [ifMatchUDPattern (TREE p ps) (RTree node qs) | qs <- sublists (length ps) subtrees]
  TRUE -> True
  ARG pos deprel -> ifMatchUDPattern (AND [POS pos, DEPREL deprel]) tree
  DEPTH_EQUALS d -> depthRTree tree == d
  DEPTH_UNDER d -> depthRTree tree < d
  DEPTH_OVER d -> depthRTree tree > d


findMatchingUDSequence :: Bool -> [UDPattern] -> UDTree -> Maybe UDTree
findMatchingUDSequence strict ps tree 
  | null ps = return tree
  | length ps > length nodes = Nothing
  | otherwise =   --- makes sense only for node-matching patterns
       case [snodes |
             snodes <- parts (length ps) nodes,
             all (\ (p,n) -> ifMatchUDPattern p (RTree n [])) (zip ps snodes)
             ] of
         snodes:_ -> smallestSpanningUDSubtree (begin snodes) (end snodes) tree
         _ -> Nothing
 where
  nodes = udWordLines (udTree2sentence tree)
  parts = if strict then segments else sublists
  begin ns = udPosition (udID (head ns)) -- exists because ps > 0
  end ns = udPosition (udID (last ns))


data UDReplacement =
    REPLACE UDPattern UDPattern
  | UNDER UDPattern UDReplacement
  | PRUNE UDPattern  -- drop dependents, shorthand for FLATTEN p 0
  | FILTER_SUBTREES UDPattern UDPattern -- keep only subtrees that match the second pattern
  | FLATTEN UDPattern Int -- cut the tree at depth Int
  | CHANGES [UDReplacement] -- try different replacements in this order, break after first applicable
  | COMPOSE [UDReplacement] -- make all changes one after the other
 deriving (Show,Read)

replaceWithUDPattern :: UDReplacement -> UDTree -> (UDTree,Bool)
replaceWithUDPattern rep tree@(RTree node subtrs) = case rep of
  REPLACE cond change | ifMatchUDPattern cond tree -> true $ case change of
    FORM s -> tree{root = node{udFORM = s}}
    LEMMA s -> tree{root = node{udLEMMA = s}}
    POS s -> tree{root = node{udUPOS = s}}
    DEPREL s -> tree{root = node{udDEPREL = s}}
    AND ps -> case ps of
      p:pp -> let
                (tr,_)  = replaceWithUDPattern (REPLACE TRUE p) tree
              in fst (replaceWithUDPattern (REPLACE TRUE (AND pp)) tr)
      _ -> tree
  UNDER cond replace | ifMatchUDPattern cond tree -> true $ tree{subtrees = map (fst . replaceWithUDPattern rep) subtrs} 
  PRUNE cond | ifMatchUDPattern cond tree -> true $ tree{subtrees = []}
  FILTER_SUBTREES cond scond | ifMatchUDPattern cond tree ->
    let sts = [st | st <- subtrs, ifMatchUDPattern scond st]
    in (RTree node sts, length sts /= length subtrs)
  FLATTEN cond depth | ifMatchUDPattern cond tree -> true $ flattenRTree depth tree
  CHANGES reps -> case reps of
    r:rs -> case replaceWithUDPattern r tree of
      (tr,True) -> (tr,True)
      _ -> replaceWithUDPattern (CHANGES rs) tree
    _ -> (tree,False)
  COMPOSE reps -> case reps of
    r:rs -> case replaceWithUDPattern r tree of
      (tr,b) -> let (tr2,bs) = replaceWithUDPattern (COMPOSE rs) tr in (tr2, b || bs)
    _ -> (tree,False)
  _ -> (tree,False)
 where
   true t = (t,True)

flattenRTree :: Int -> RTree a -> RTree a
flattenRTree d tr@(RTree node subtrs) = case d of
  0 -> RTree node []
  _ -> RTree node (map (flattenRTree (d-1)) subtrs)

smallestSpanningUDSubtree :: Int -> Int -> UDTree -> Maybe UDTree
smallestSpanningUDSubtree begin end tree = case tree of
  _ | sizeRTree tree < 1 + end - begin -> Nothing
  _ -> case [t | t <- subtrees tree, covers t] of
    t:_ -> smallestSpanningUDSubtree begin end t -- t is unique, since each node occurs once
    _ -> Just tree -- must cover due to the size condition
 where
   covers t = all (\n -> elem n [udPosition (udID w) | w <- allNodesRTree t]) [begin..end]


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

segments :: Int -> [a] -> [[a]]
segments n xs =
  let lxs = length xs in
  if n <= lxs then [take n (drop m xs) | m <- [0..lxs-n]]
  else []




 
