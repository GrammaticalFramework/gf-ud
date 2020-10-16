module UDAnalysis where

import UDConcepts
import GFConcepts
import UDAnnotations
import UDOptions

import PGF

import Data.List
import qualified Data.Map as M


-- get statistics from a tree collection: a frequency list in descending order
udFrequencies :: Opts -> [UDSentence] -> [([String],Int)]
udFrequencies opts = frequencyList . map f . concatMap udWordLines
  where
    f = \w -> [fun w | (opt,fun) <- optfuns, isOpt opts opt]
    optfuns = [
      ("FORM ", udFORM),
      ("LEMMA", udLEMMA),
      ("POS",   udUPOS),
      ("FEATS", prt . udFEATS),
      ("DISTANCE", \w -> show (udid2int (udHEAD w) - udid2int (udID w))),
      ("DEPREL", udDEPREL)
      ]

-------------------
-- another look at the UD2GF task: analyse what shapes of UD trees there are in a treebank
-- and which of them can be covered by a given grammar and annotations
-------------------

-- frequency list of UD types in a treebank
udTypeFrequencies :: [UDSentence] -> [(UDType,Int)]
udTypeFrequencies =
    frequencyList
  . map normalizeUDType
  . concatMap typesInUDTree
  . map udSentence2tree

data UDType = UDType {
  udVal  :: (POS,[UDData]),
  udArgs :: [(POS,(Label,[UDData]))]
  }
 deriving (Eq,Ord,Show)

prUDType ut = unwords $ show (udVal ut) : map show (udArgs ut)

typesInUDTree :: UDTree -> [UDType]
typesInUDTree tr@(RTree un uts) =
  UDType (udUPOS un,udFEATS un) [(udUPOS n,(udDEPREL n, udFEATS n)) | RTree n _ <- uts] :
  concatMap typesInUDTree uts

-- ignore argument order ---- and morphological tags
normalizeUDType :: UDType -> UDType
normalizeUDType ut = ut {
  udArgs = sort [(p,(l,m)) | (p,(l,m)) <- udArgs ut]
  }

-- a type with many arguments can subsume a type with few arguments; returns the remaining ones of the many
subsumeUDType :: UDType -> UDType -> Maybe [(POS,(Label,[UDData]))]
subsumeUDType many few =
  if (match (headArg few) (headArg many))                      -- value types match
    then findAll (sort (udArgs many)) (sort (udArgs few))  -- argument types are found
    else Nothing
 where
   findAll ms fs = case fs of
     x:xs -> case partition (match x) ms of
       (m:mm,nn) -> findAll (mm ++ nn) xs
       _ -> Nothing
     _ -> Just ms
   match (pos,(label,feats)) (mpos,(mlabel,mfeats)) =
     mpos == pos && mlabel == label &&
     all (flip elem mfeats) feats  -- few has the sought features, many can contain more
   headArg (UDType (pos,feats) _) = (pos,(head_Label, feats))
   
-- many subsumed types can together exactly cover a type with many arguments: output Just [] in that case
--- NB starting with wrong fews may result in unwanted residual arguments that could be covered otherwise
coverUDType :: UDType -> [UDType] -> Maybe [(POS,(Label,[UDData]))]
coverUDType many fews = case fews of
  f:fs -> case subsumeUDType many f of
    Just ms -> coverUDType many{udArgs = ms} fs
    _ -> Nothing
  _ -> Just (udArgs many)

-------------------------------------
-- matching with GF types

labelled2udTypes :: UDEnv -> LabelledType -> [UDType]
labelled2udTypes env (val,args) = [
  UDType (pos,fs) udargs |
     udargs0  <- sequence [ [(pos,(lab,feats)) |  pos <- allPosOfCat env cat] | (cat,(lab,feats)) <- args],
     (pos,fs) <- [(pos,fs) | (pos,("head",fs)) <- udargs0], --- NB: there is always exactly one head
     elem pos (allPosOfCat env val),
     let udargs = [arg | arg@(_,(l,_)) <- udargs0, l /= head_Label]
    ]

-- POS tags of lexical categories reachable by unary functions
allPosOfCat :: UDEnv -> Cat -> [POS]
allPosOfCat env cat0 = nub [
   pos |
     cat <- nub (expands cat0),
     Just pos <- [lookup cat possLexCat]
  ]
 where
  unaries = [(val,arg) | fun@(f,(val,[(arg,_)])) <- allFunsEnv env]
  possLexCat =
    M.assocs (catLabels (absLabels env)) ++
    M.assocs (auxCategories (cncLabels env))
  expands cat = cat : [cat2 | (c,cat1) <- unaries, c==cat, cat2 <- expands cat1]

-------------------------------------
-- descending sorted frequency list of anything, e.g. types in UD trees
frequencyList :: Ord a => [a] -> [(a,Int)]
frequencyList xs = sortOn ((0-) . snd) $ M.assocs $ M.fromListWith (+) [(x,1) | x <- xs]

