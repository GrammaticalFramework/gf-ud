module UDPatternsOnly where

import RTree
import UDConcepts
---import GFConcepts
import UDAnalysisOnly
import UDOptions
import Data.Maybe (listToMaybe)
import Data.List (intercalate)


showMatchesInUDSentence :: Opts -> UDPattern -> UDSentence -> String
showMatchesInUDSentence opts p s =
  if   null matches then ""
  else unlines (udCommentLines s ++ matches)
 where
   matches = [prt (adjust t) | t <- matchesUDPattern p (udSentence2tree s)]
   adjust
     | isOpt opts "adjust" = adjustUDIds . udTree2sentence . createRoot
     | isOpt opts "prune" = udTree2sentence . (\t -> t{subtrees = []})
     | otherwise = udTree2sentence

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
   ns = adjustUDIds $ udTree2sentence $ createRoot tr
   (tr,changed) = replacementsWithUDPattern rep (udSentence2tree s)

replacementsWithUDPattern :: UDReplacement -> UDTree -> (UDTree,Bool)
replacementsWithUDPattern rep tree = case replaceWithUDPattern rep tree of
  (RTree node subtrs,b) -> let (trs,bs) = unzip (map (replacementsWithUDPattern rep) subtrs)
                           in (RTree node trs, or (b:bs))

data UDPattern =
    FORM String
  | LEMMA String
  | POS String
  | XPOS String
  | MISC String String
  | FEATS String  -- feature list matches exactly
  | FEATS_ String -- a sublist of features matches exactly
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
  | PROJECTIVE
  | ARG String String
  | DEPTH Int
  | DEPTH_UNDER Int
  | DEPTH_OVER Int
  | LENGTH Int
  | LENGTH_UNDER Int
  | LENGTH_OVER Int
 deriving (Show,Read,Eq)

ifMatchUDPattern :: UDPattern -> UDTree -> Bool
ifMatchUDPattern patt tree@(RTree node subtrees) = case patt of
  FORM s -> matchString s (udFORM node)
  LEMMA s -> matchString s (udLEMMA node)
  POS s -> matchString s (udUPOS node)
  XPOS s -> matchString s (udXPOS node)
  MISC name s -> maybe False (matchString s) $ listToMaybe [ intercalate "," vals | UDData arg vals <- udMISC node , arg == name  ]
  FEATS udds -> udFEATS node == prs udds
  FEATS_ udds -> 
    let uddlist = prs udds in
    or [fs == uddlist | fs <- sublists (length uddlist) (udFEATS node)]
  DEPREL s -> matchString s (udDEPREL node)
  DEPREL_ s -> matchString s (takeWhile (/=':') (udDEPREL node))
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
  PROJECTIVE -> isProjective tree
  ARG pos deprel -> ifMatchUDPattern (AND [POS pos, DEPREL deprel]) tree
  DEPTH d -> depthRTree tree == d
  DEPTH_UNDER d -> depthRTree tree < d
  DEPTH_OVER d -> depthRTree tree > d
  LENGTH d -> length (allNodesRTree tree) == d
  LENGTH_UNDER d -> length (allNodesRTree tree) < d
  LENGTH_OVER d -> length (allNodesRTree tree) > d

matchString p s = case p of
  '*':pp -> pp == drop (length s - length pp) s
  _:_ | last p =='*' -> init p == take (length (init p)) s
  _ -> p == s
  
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
    REPLACE_FORM String String
  | REPLACE_LEMMA String String
  | REPLACE_POS String String
  | REPLACE_XPOS String String
  | REPLACE_MISC String String String
  | REPLACE_DEPREL String String
  | REPLACE_DEPREL_ String String
  | REPLACE_FEATS String String
  | REPLACE_FEATS_ String String
  | IF UDPattern UDReplacement
  | UNDER UDPattern UDReplacement
  | OVER UDPattern UDReplacement
  | PRUNE UDPattern Int -- drop dependents down to depth Int
  | FILTER_SUBTREES UDPattern UDPattern -- keep only subtrees that match the second pattern
  | FLATTEN UDPattern -- lift dependents of dependents to the same level as dependents
  | RETARGET UDPattern UDPattern UDPattern -- retarget subtrees that satisfy patt1 to their (first) sister that patt2  
  | CHANGES [UDReplacement] -- try different replacements in this order, break after first applicable
  | COMPOSE [UDReplacement] -- make all changes one after the other
 deriving (Show,Read,Eq)

replaceWithUDPattern :: UDReplacement -> UDTree -> (UDTree,Bool)
replaceWithUDPattern rep tree@(RTree node subtrs) = case rep of
  REPLACE_FORM old new | ifMatchUDPattern (FORM old) tree -> true $ tree{root = node{udFORM = new}}
  REPLACE_LEMMA old new | ifMatchUDPattern (LEMMA old) tree -> true $ tree{root = node{udLEMMA = new}}
  REPLACE_POS old new | ifMatchUDPattern (POS old) tree -> true $ tree{root = node{udUPOS = new}}
  REPLACE_XPOS old new | ifMatchUDPattern (XPOS old) tree -> true $ tree{root = node{udXPOS = new}}
  REPLACE_MISC name old new | ifMatchUDPattern (MISC name old) tree -> true $ tree{root = node{udMISC = map (\ud -> if udArg ud == name then ud{udVals = getSeps ',' new} else ud) (udMISC node)}}
  REPLACE_DEPREL old new | ifMatchUDPattern (DEPREL old) tree -> true $ tree{root = node{udDEPREL = new}}
  REPLACE_DEPREL_ old new | ifMatchUDPattern (DEPREL_ old) tree -> true $ tree{root = node{udDEPREL = new}}
  REPLACE_FEATS old new | ifMatchUDPattern (FEATS old) tree -> true $ tree{root = node{udFEATS = prs new}}
  REPLACE_FEATS_ old new | ifMatchUDPattern (FEATS_ old) tree -> true $
    let news = [(udArg fv, udVals fv) | fv <- prs new] in
    tree{root = node{udFEATS = [maybe fv (\v -> fv{udVals = v}) (lookup (udArg fv) news) | fv <- udFEATS node]}}
  IF cond replace | ifMatchUDPattern cond tree -> replaceWithUDPattern replace tree
  UNDER cond replace | ifMatchUDPattern cond tree -> true $ tree{subtrees = map (fst . replaceWithUDPattern replace) subtrs} 
  OVER cond replace | any (ifMatchUDPattern cond) subtrs -> replaceWithUDPattern replace tree
  PRUNE cond depth | ifMatchUDPattern cond tree -> true $ flattenRTree depth tree
  FILTER_SUBTREES cond scond | ifMatchUDPattern cond tree ->
    let sts = [st | st <- subtrs, ifMatchUDPattern scond st]
    in (RTree node sts, length sts /= length subtrs)
  RETARGET cond patt1 patt2 | ifMatchUDPattern cond tree ->
    let
      newhead = [subtr | subtr <- subtrs, ifMatchUDPattern patt2 subtr]
      retarget st = case newhead of
        subtr:_ | udID (root st) == udID (root subtr) ->
          [subtr{subtrees = subtrees subtr ++
              [t{root = (root t){udHEAD = udID (root subtr)}} | t <- subtrs, ifMatchUDPattern patt1 t] }]
        _:_ | ifMatchUDPattern patt1 st -> []
        _ -> [st]
      sts = concat [retarget st | st <- subtrs]
    in (RTree node sts, length sts /= length subtrs)
  FLATTEN cond | ifMatchUDPattern cond tree ->
    let sts = concat
                [subtr{subtrees = []} :
                  [t{root = (root t){udHEAD = udID node}} | t <- subtrees subtr]
                                 | subtr <- subtrs]
    in (RTree node sts, length sts /= length subtrs)
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




 
