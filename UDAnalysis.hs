module UDAnalysis where

import UDConcepts
import GFConcepts
import UDAnnotations
import UDOptions

import PGF

import Data.List
import Data.Char
import qualified Data.Map as M


-- get statistics from a tree collection: a frequency list in descending order
udFrequencies :: Opts -> [UDSentence] -> [([String],Int)]
udFrequencies opts sents =
   if isOpt opts "SUBTREETYPE"
   then [([prUDType ut],n) | (ut,(n,_)) <- udTypeFrequencies sents, length (udArgs ut) > 1]
   else addTotal $ sortOn ((0-) . snd) $ M.assocs $ udFrequencyMap opts sents
  where
    allWords = concatMap udWordLines sents
    total = length allWords
    addRelative (k,n) = (k,(n, fromIntegral n / fromIntegral total)) ----
    addTotal = ([(["TOTAL SENTENCES"],length sents),(["TOTAL WORDS"],total)]++)

udFrequencyMap :: Opts -> [UDSentence] -> M.Map [String] Int
udFrequencyMap opts sents =
   if isOpt opts "SUBTREETYPE"
   then M.fromList [([prUDType ut],n) | (ut,n) <- M.assocs (udTypeFrequencyMap sents)]
   else frequencyMap $ map f $ allWords
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
    allWords = concatMap udWordLines sents

-- cosine similarity of two treebanks
udCosineSimilarity :: Opts -> [UDSentence] -> [UDSentence] -> Double
udCosineSimilarity opts xs ys = cosineSimilarityOfMaps fxs fys
  where
    fxs = udFrequencyMap opts xs
    fys = udFrequencyMap opts ys

-------------------
-- another look at the UD2GF task: analyse what shapes of UD trees there are in a treebank
-- and which of them can be covered by a given grammar and annotations
-------------------

-- frequency list of UD types in a treebank
udTypeFrequencyMap :: [UDSentence] -> M.Map UDType Int
udTypeFrequencyMap ss =
    frequencyMap nexx
  where
    nexx = [normalizeUDType ty  | (ty,_) <- exx]
    exx = concatMap (typesInUDTree . udSentence2tree) ss

udTypeFrequencies :: [UDSentence] -> [(UDType,(Int,String))]
udTypeFrequencies ss =
    frequencyExampleList nexx
  where
    nexx = [(normalizeUDType ty, ex)  | (ty,ex) <- exx]
    exx = concatMap (typesInUDTree . udSentence2tree) ss

data UDType = UDType {
  udVal  :: (POS,(Label,[UDData])),
  udArgs :: [(POS,(Label,[UDData]))] -- including the val, to keep its position
  }
 deriving (Eq,Ord,Show)

prUDType ut = unwords $ intersperse "->" $ map prOne (udArgs ut ++ [udVal ut])
  where
    prOne (pos,(label,_)) = pos ++ "(" ++ label ++ ")"

typeOfUDTree :: UDTree -> UDType
typeOfUDTree tr@(RTree un uts) =
  UDType tun
         (map snd (sort (ptun:[(udID n, (udUPOS n,(udDEPREL n, udFEATS n))) | RTree n _ <- uts])))
 where
   (position,tun) = (udID un, (udUPOS un,(udDEPREL un, udFEATS un)))
   ptun = (udID un, (udUPOS un,(head_Label, udFEATS un)))

typesInUDTree :: UDTree -> [(UDType,String)] -- type and example
typesInUDTree tr =
  (typeOfUDTree tr, topStringOfUDTree tr) :
  concatMap typesInUDTree (subtrees tr)

-- ignore argument order - and morphological tags with [] instead of m
normalizeUDType :: UDType -> UDType
normalizeUDType ut = ut {
  udVal  = head [(p,(l,[])) | (p,(l,m)) <- [udVal ut]],
  udArgs = {-sort-} [(p,(l,[])) | (p,(l,m)) <- udArgs ut]
  }

matchUDType :: UDType -> UDType -> Bool
matchUDType sought tried = normalizeUDType sought == normalizeUDType tried ---- TODO: more precision

findUDTypeInTree :: UDType -> UDTree -> [UDTree]
findUDTypeInTree ty tr@(RTree un uts) =
  [tr | matchUDType ty (typeOfUDTree tr) ] ++
  concatMap (findUDTypeInTree ty) uts

topStringOfUDTree :: UDTree -> String
topStringOfUDTree (RTree n ts) = unwords $ map udFORM $ sortOn udID $ n : map root ts

{- --- not used, no longer valid
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
-}

-------------------------------------
-- matching with GF types

{- --- not used, no longer valid
labelled2udTypes :: UDEnv -> LabelledType -> [UDType]
labelled2udTypes env (val,args) = [
  UDType (pos,(root_Label,fs)) udargs |  --- rootLabel?
     udargs0  <- sequence [ [(pos,(lab,feats)) |  pos <- allPosOfCat env cat] | (cat,(lab,feats)) <- args],
     (pos,fs) <- [(pos,fs) | (pos,("head",fs)) <- udargs0], --- NB: there is always exactly one head
     elem pos (allPosOfCat env val),
     let udargs = [arg | arg@(_,(l,_)) <- udargs0, l /= head_Label]
    ]
-}

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
    [(c,p) | (c,(p,_)) <- M.assocs (catLabels (absLabels env))] ++
    M.assocs (auxCategories (cncLabels env))
  expands cat = cat : [cat2 | (c,cat1) <- unaries, c==cat, cat2 <- expands cat1]

-------------------------------------
-- descending sorted frequency list of anything, e.g. types in UD trees
frequencyList :: Ord a => [a] -> [(a,Int)]
frequencyList xs = sortOn ((0-) . snd) $ M.assocs $ frequencyMap xs

frequencyMap :: Ord a => [a] -> M.Map a Int
frequencyMap xs = M.fromListWith (+) [(x,1) | x <- xs]

frequencyExampleList :: Ord a => [(a,b)] -> [(a,(Int,b))]
frequencyExampleList xs = sortOn ((0-) . fst . snd) $ M.assocs $ M.fromListWith add [(x,(1,e)) | (x,e) <- xs]
  where
    add (n,e) (m,_) = (n+m,e)

cosineSimilarityOfMaps :: Ord a => M.Map a Int -> M.Map a Int -> Double
cosineSimilarityOfMaps fxs fys = fromIntegral (scalarProduct fxs fys) / (size fxs * size fys)
  where
    scalarProduct fxs fys = sum [x*y | (w,x) <- M.assocs fxs, let y = maybe 0 id (M.lookup w fys)]
    size fs = sqrt (fromIntegral (sum [x*x | (_,x) <- M.assocs fs]))

cosineSimilarity :: Ord a => [a] -> [a] -> Double
cosineSimilarity xs ys = cosineSimilarityOfMaps fxs fys
  where
    fxs = M.fromListWith (+) [(x,1) | x <- xs]
    fys = M.fromListWith (+) [(x,1) | x <- ys]
    scalarProduct fxs fys = sum [x*y | (w,x) <- M.assocs fxs, let y = maybe 0 id (M.lookup w fys)]
    size fs = sqrt (fromIntegral (sum [x*x | (_,x) <- M.assocs fs]))

-----------------------------------------------------
-- lexical entries obtained from lemma + primary cat

lexicalEntries :: UDEnv -> [UDSentence] -> [((String, String),Int)] -- lemma, cat, #occurrences
lexicalEntries env uds = M.assocs (M.fromListWith (+) [(wc,1) | Just wc <- map entry allwords])
 where
  allwords = concatMap udWordLines uds
  entry udw = case M.lookup (udUPOS udw) (catsForPOS env) of
    Just cps -> case [c | Left (c,True) <- cps] of
      cat:_  -> Just (udLEMMA udw, showCId cat)
      _ -> Nothing
    _ -> Nothing


----------------------------------------------------
-- an analysis of UD types and their conversion to GF

getUDTypesFromFile file = do
  uds <- parseUDFile file
  let typs = udTypeFrequencies uds
  return typs

testUDTypes n file = do
  typs <- getUDTypesFromFile file
  print $  length typs
  let gftyps = [(suggestAbsType gft, (gft, (i,e))) | (t,(i,e)) <- typs, i >= n, length (udArgs t) > 1, let gft = ud2gfType t]
  print $ length gftyps
  let sgftyps = sortOn (\ (ty,(_,(i,_))) -> ty) gftyps
  let gsgftyps = groupBy (\ (xty,_) (yty,_) -> xty == yty) sgftyps
  let sgsgftyps = concat $ sortOn (\ ((_,(_,(i,_))):_) -> 0-i) gsgftyps
--  let ssgftyps = 
  putStrLn $ unlines $ map (\ (gft, ((t,(l,ls)),(i,e))) ->
    "-- " ++ prAbsType gft ++ " ; -- " ++ unwords (ls ++ [show i]) ++ " ; " ++ l ++ {- " " ++ prAbsType t ++ -} " ; " ++ e) $ sgsgftyps

ud2gfType :: UDType -> (AbsType,(Label,[Label]))
ud2gfType udt@(UDType uval uargs) = ((val,args),labels)
  where
    val  = pos2catVal uval
    args = map pos2cat (uargs)
    labels = (fst (snd (uval)), map (fst . snd) uargs)

    pos2cat (pos,(lab,_)) = mkCId pos
    pos2catVal (pos,(lab,_)) = mkCId pos

--  udVal  :: (POS,(Label,[UDData])),
--  udArgs :: [(POS,(Label,[UDData]))]

suggestAbsType :: (AbsType,(Label,[Label])) -> AbsType
suggestAbsType ((val,args),(hlabel,labels)) = (suggestCat val hlabel, map (uncurry suggestCat) (zip args labels))
 where
   suggestCat v hl = case hl of
     _ | elem hl npLabels -> mkCId "NP"
     _ | elem hl ["nmod","obl"] -> mkCId "PP"
     _ | elem hl ["advmod"] -> mkCId "Adv"
     _ | elem hl ["nummod"] -> mkCId "Card"
     _ -> case showCId v of
       "NOUN" -> mkCId "CN"
       "ADJ"  -> mkCId "AP"
       "VERB" -> mkCId "VP"
       "DET"  -> mkCId "Det"
       "ADP"  -> mkCId "Prep"
       "ADV"  -> mkCId "Adv"
       "PRON"  -> mkCId "Pron"
       "PROPN"  -> mkCId "PN"
       "SCONJ" -> mkCId "Subj"
       "CCONJ" -> mkCId "Conj"
       "PUNCT" -> mkCId "Punct"
       "INTJ" -> mkCId "Interj"
       "NUM" -> mkCId "Card"
       _ -> v
   npLabels = ["nsubj","obj","iobj"]

-- to produce configs from post-edited GF file:

mkConfigs file = do
  fs <- readFile file >>= return . map words . lines
  let fls ws = [
        w:ls | w:ww <- [ws],
        head w /= '-', elem "--" ww,
        let _:ls = takeWhile (not . Data.Char.isDigit . head) (dropWhile (/="--") ww)
        ]
  putStrLn $ unlines $ map (unwords . ("#fun" :)) $ filter (not . null) $ concatMap fls fs

mkProbs max_digits file = do -- max_digits = 4 for en_ewt_train, i.e. 1000's of occurrences
  fs <- readFile file >>= return . map words . lines
  let fls ws = [
        fun ++ [n] |
          (mfun,_:ww) <- [break (==";") ws],
          n:_ <- [[v | v <- ww, all isDigit v]],
          not (isPrefixOf "---" (head mfun)),
          let fun = if head mfun == "--" then [] else mfun
        ]
  let pfs = concatMap fls fs
  let collect xs = case xs of
        x:xs -> case span ((==1) . length) xs of
           ([],[]) -> []
           (xx,xxx) -> (concat (x:xx)) : collect xxx
        _ -> []
  let mkSum ws =
        let
          (f,ns) = break (all isDigit) ws
          sns = sum [(read n) :: Int | n <- ns]
        in take 1 f ++ ["0." ++ replicate (max_digits - length (show sns)) '0' ++ show sns]
  putStrLn $ unlines $ map unwords $ map mkSum (collect pfs)

