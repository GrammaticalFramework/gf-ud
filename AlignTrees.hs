module AlignTrees where

import UDConcepts
import GFConcepts
import Data.List
import Data.List.Split
import qualified Data.MultiSet as MS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

-- | Data type definitions etc.

-- an alignment is a pair of corresponding subtrees (not a type synonym just
-- because of custom Eq instance)
newtype Alignment = A (UDTree,UDTree)

instance Show Alignment where
  show (A a) = show a -- show as pair

-- prAlignment is used instead of show to "visually" inspect the alignments
-- (and to compare them ignoring the context)
prAlignment :: Alignment -> String
prAlignment (A (t,u)) = t' ++ "|" ++ u'
  where (t',u') = (dropWhile (== ' ') $ prDepTreeString t, 
                   dropWhile (== ' ') $ prDepTreeString u)

-- two alignment are considered equal whenever their "linearization" is the
-- same (so to ignore all info excepts words and their order)
instance Eq Alignment where
  a == b = prAlignment a == prAlignment b

-- alignments can be ordered based on:
-- 1. size of their left tree
-- 2. "linearization" of their left tree (alphabetical ord.)
-- 3. size of their right tree
-- 4. "linearization" of their right tree (alphabetical ord.)
-- needed by M.fromListWith and M.union(s)With
instance Ord Alignment where
  (A (t1,u1)) <= (A (t2,u2)) = k t2 u2 <= k t1 u1
    where 
      k t u = (sizeRTree t,prDepTreeString t,sizeRTree u,prDepTreeString u)

-- alignments are a mapping with key k :: Alignment, 
-- value a :: (set of reasons, n_occurrences)
type Vals = (S.Set Reason,Int) -- TODO: rename
type Alignments = M.Map Alignment Vals

-- helper function used to combine reasons and n_occurrences whenever using
-- union(s)With, fromListWith etc. 
combineVals :: Vals -> Vals -> Vals
combineVals (r,n) (s,m) = (r `S.union` s,n + m)

-- possible reasons for subtree alignment
data Reason = DIV   -- known interlingual divergence
            | UD    -- matching root udlabel
            | POS   -- POS-equivalence
            | PASS  -- different voice, used together with DIV
            | HEAD  -- composed of the heads of another alignment (alignHeads)
            | PREV  -- already found in another sentence 
            | FAST  -- found by fast_align
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- type of criteria for alignment. Each criterion is composed of
data Criterion = C {
  -- a function telling when two dep. trees should be aligned
  func :: UDTree -> UDTree -> Bool, 
  -- the reasons associated with such criterion 
  reas :: S.Set Reason,
  -- a flag telling whether heads should also be aligned (i.e. if align
  -- should call alignHeads)
  flag :: Bool
}

-- | Criteria

-- original basic criterion: matching root UD labels 
udMatch :: UDTree -> UDTree -> Bool
(RTree n ts) `udMatch` (RTree m us) = udSimpleDEPREL n == udSimpleDEPREL m

-- POS-equivalence
posEquiv :: UDTree -> UDTree -> Bool
t1 `posEquiv` t2 = (not . null) ct1 && (ct1 == ct2)
  where (ct1, ct2) = (contentTags t1, contentTags t2)
  
-- an adverb is translated as a PP (instance of structural divergence)
advmodObl :: UDTree -> UDTree -> Bool
advmodObl t u = isLabelled "advmod" t && isLabelled "obl" u
                -- POS-equiv would be too strict here
                && length (contentTags t) == length (contentTags u)

-- an adjective is translated as a nmod (instance of categorial divergence)
-- TODO: decide whether to use this or not
amodNmod :: UDTree -> UDTree -> Bool
amodNmod t u = isLabelled "amod" t && isLabelled "nmod" u
                && subtreesTags t == subtreesTags u 

-- an adjective is translated as an adverb (instance of categorial divergence)
-- TODO: decide whether to use this or not
amodAdvmod :: UDTree -> UDTree -> Bool
amodAdvmod t u = isLabelled "amod" t && isLabelled "advmod" u
                && subtreesTags t == subtreesTags u

-- a verb is transitive language A but not in language B
-- (structural divergences regarding object)
objObl :: UDTree -> UDTree -> Bool
objObl t u = isLabelled "obj" t && isLabelled "obl" u
             && t `posEquiv` u

-- the indirect object of a verb in language A is rendered as a PP language B
-- (structural divergences regarding indirect object)
iobjObl :: UDTree -> UDTree -> Bool
iobjObl t u = isLabelled "iobj" t && isLabelled "obl" u
              && t `posEquiv` u

-- the indirect object of a sentence is the object in its translation
iobjObj :: UDTree -> UDTree -> Bool
iobjObj t u = isLabelled "iobj" t && isLabelled "obj" u
             && t `posEquiv` u

-- a verb is transitive language A but not in language B
-- (structural divergences regarding subject)
nsubjObl :: UDTree -> UDTree -> Bool
nsubjObl t u = isLabelled "nsubj" t && isLabelled "obl" u
             && t `posEquiv` u

passSubjObj :: UDTree -> UDTree -> Bool
-- t's label is checked subtypes included, so isLabelled can't be used
passSubjObj t u = udDEPREL (root t) == "nsubj:pass" && isLabelled "obj" u
              && t `posEquiv` u

passOblSubj :: UDTree -> UDTree -> Bool
passOblSubj t u = isLabelled "obl" t && isLabelled "nsubj" u
                  && t `posEquiv` u

ud, pos, divs, udpos, pass :: Criterion
ud = C udMatch (S.singleton UD) True
pos = C posEquiv (S.singleton POS) True    
udpos = C (\t u -> udMatch t u && posEquiv t u) (S.fromList [UD,POS]) True
divs = C (\t u -> 
  or [
    advmodObl t u, advmodObl u t, 
    objObl t u, objObl u t,
    iobjObj t u, iobjObj u t,
    nsubjObl t u, nsubjObl u t,
    --iobjObl t u, iobjObl u t,
    amodNmod t u, amodNmod u t,
    amodAdvmod t u, amodAdvmod u t
  ]) (S.singleton DIV) False 
pass = C (\t u -> or [passSubjObj t u, passSubjObj u t,
                      passOblSubj t u, passOblSubj u t]) 
          (S.fromList [PASS, DIV]) True

criteria :: [Criterion]
criteria = [udpos, ud, divs, pass, pos]

-- | Alignment functions

-- main alignment function. Works on a list of pairs of CORRESPONDING dep.
-- trees (i.e. it does NOT perform sentence alignment)  
align :: [Criterion] -> [(UDTree,UDTree)] -> Alignments
align = align' M.empty
  where
    align' :: Alignments -> [Criterion] -> [(UDTree,UDTree)] -> Alignments
    align' as _ [] = as
    align' as cs (tu:tus) = 
      align' (M.unionWith combineVals as (alignSent as cs tu)) cs tus

-- sentence-level alignment function: given a list of criteria (sorted by 
-- priority) and a pair of dep. trees, return the corresponding map of 
-- alignments, reusing known alignments as well
alignSent :: Alignments -> [Criterion] -> (UDTree,UDTree) -> Alignments
alignSent as cs (t,u) = M.fromListWith combineVals (alignSent' as cs (t,u))
  where
    -- the actual recursive function works with a list of (key,val) pairs 
    -- instead, for simplicity TODO: simplify code by not having a helper 
    alignSent' as cs (t@(RTree n ts), u@(RTree m us))
      -- 1+ criteria match
      | null cs' = prune as'
      -- the alignment is known TODO: handle heads better
      | A (t,u) `M.member` as = [
          (A (t,u),(S.singleton PREV,1)), 
          (alignHeads (A (t,u)),(S.fromList [HEAD, PREV], 1))
        ]
      | otherwise = []
        where
          -- new alignments, subtrees included
          as' = atu' ++ sas
            where 
              -- (t,u) alignment(s)
              atu' = if flag c then [atu,hAtu] else [atu]
              atu = (A (t,u),(reas c,1))
              hAtu = (alignHeads $ fst atu,(S.insert HEAD (reas c), 1))
              -- 1st applying criterion
              c = head cs'
              -- subtree alignments
              sas = concatMap (alignSent' as cs) [(t,u) | t <- ts', u <- us']
                where (ts',us') = (sortTs ts,sortTs us)
           -- sort subtrees by root label
          sortTs = sortOn (udSimpleDEPREL . root)
          -- applying criteria
          cs' = map snd (filter fst (zip [f t u | f <- map func cs] cs))

-- helper function removing the less valid alternative alignments
prune :: [(Alignment,Vals)] -> [(Alignment,Vals)]
prune = nubBy areAlt . sortAligns
  where 
    -- check if two alignments are alternative to each other comparing THE
    -- ACTUAL TREES, and not their "linearizations"
    areAlt (A (t1,u1),_) (A (t2,u2),_) = t1 == t2 || u1 == u2
    -- sort alignments by number of reasons (decr.), then by first (-> highest
    -- priority) reason
    sortAligns = sortOn (\(_,(rs,n)) -> 
      let rs' = rs `S.difference` S.fromList [HEAD,PREV] 
      in (-(length rs'), last $ S.elems rs))

-- head alignment: given an alignment of dep. trees, return a new one for 
-- their "heads", but respecting any compounds and aux+verbs (and more?)
alignHeads :: Alignment -> Alignment
alignHeads (A (RTree n ts,RTree m us))
  -- if there are compound constructions, look for their counterparts and
  -- align accordingly
  | (not . null) cts = A (RTree n cts, RTree m (compCounterparts ts us))
  | (not . null) cus = A (RTree n (compCounterparts us ts), RTree m cus)
  -- if the roots are verbs (to avoid messing with copulas) and only one of
  -- them has 1+ auxiliaries, align verb | verb + auxs
  | all isVerb [n,m] && (not . null) ats && null aus = A (RTree n ats, RTree m [])
  | all isVerb [n,m] && null ats && (not . null) aus = A (RTree n [], RTree m aus) 
  | otherwise = A (RTree n [], RTree m [])
  where
    -- select subtrees labelled in a certain way 
    filterByLabel l xs = filter (isLabelled l) xs

    ats = filterByLabel "aux" ts
    aus = filterByLabel "aux" us
    cts = filterByLabel "compound" ts
    cus = filterByLabel "compound" us

    -- given two lists of subtrees, select those that, in the second,
    -- could correspond to a compound construction in the first, i.e.
    compCounterparts :: [UDTree] -> [UDTree] -> [UDTree]
    compCounterparts ts us = cus ++ fus' ++ nus' ++ aus'
      where
        cus = filterByLabel "compound" us
        fus' = if length fus > length fts then fus else []
          where 
            fus = filterByLabel "flat" us
            fts = filterByLabel "flat" ts
        nus' = if length nus > length nts then nus else []
          where
            nus = filterByLabel "nmod" us
            nts = filterByLabel "nmod" ts
        aus' = if length aus > length ats then aus else []
          where
            aus = filterByLabel "amod" us
            ats = filterByLabel "amod" ts  

-- | POS-utils

-- multiset of the POS tags contained in a dep. tree
contentTags :: UDTree -> MS.MultiSet POS
contentTags = MS.fromList . filter relevant . map udUPOS . allNodesRTree
  where relevant p = p `elem` openPOS ++ ["NUM"] -- TODO: pronpuns?

-- multiset of content tags of the subtrees of t
subtreesTags :: UDTree -> MS.MultiSet POS
subtreesTags t = MS.unions (map contentTags (subtrees t))

-- POS: open classes
openPOS :: [POS]
openPOS = ["ADJ", "ADV", "INTJ", "NOUN", "PROPN", "VERB"]

-- check if a node is a verb
isVerb :: UDWord -> Bool
isVerb h = udUPOS h == "VERB" || udUPOS h == "AUX"

-- | UD-utils

isPassive :: UDTree -> Bool
isPassive s = "pass" `elem` map (last . splitOn ":" . udDEPREL) (allNodesRTree s)

-- check if the label of the root of a tree is l
isLabelled :: Label -> UDTree -> Bool
isLabelled l = (== l) . udSimpleDEPREL . root

-- get label without subtypes
udSimpleDEPREL :: UDWord -> Label
udSimpleDEPREL = takeWhile (/= ':') . udDEPREL