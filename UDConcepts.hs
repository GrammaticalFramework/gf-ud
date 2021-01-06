{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module UDConcepts where

-- AR 2019-11-14 implementing
-- https://universaldependencies.org/format.html

import Gfud.RTree
import Gfud.UDStandard

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Char

-- text paragraph representing the tree of a sentence
data UDSentence = UDSentence {
  udCommentLines :: [String], -- all comments start with '#'
  udWordLines    :: [UDWord]
  }

data UDId =
    UDIdInt Int
  | UDIdRange Int Int
  | UDIdEmpty Float -- "may be a decimal number for empty nodes (lower than 1 but greater than 0)"
  | UDIdRoot  -- 0
  | UDIdNone -- _
   deriving (Eq,Ord,Show)

-- tab-separated text line describing a word
data UDWord = UDWord {
  udID     :: UDId,     -- word position
  udFORM   :: String,   -- surface form
  udLEMMA  :: String,   -- 
  udUPOS   :: POS,      --
  udXPOS   :: String,   -- language-specific pos tag, can be _
  udFEATS  :: [UDData], -- morphological features
  udHEAD   :: UDId,     -- the head of this word
  udDEPREL :: Label,    -- the label of this word
  udDEPS   :: String,   -- "Enhanced dependency graph in the form of a list of head-deprel pairs"
  udMISC   :: [UDData]  -- "any other annotation"
  } deriving (Show,Eq,Ord)

type POS = String
type Label = String

-- useless because one could just use isSubRTree, but...
isSubUDTree :: UDTree -> UDTree -> Bool
isSubUDTree t u = t == u || any (isSubUDTree t) (subtrees u)

-- it is here to show the difference with isSubUDTree', which ignores a few
-- fields (used for propagation)
isSubUDTree' :: UDTree -> UDTree -> Bool
isSubUDTree' t u = t =~ u || any (isSubUDTree' t) (subtrees u)

data UDData = UDData {
  udArg  :: String,
  udVals ::[String]
  } -- Arg=Val,Val,Val
   deriving (Eq,Show,Ord)

initUDWord :: Int -> UDWord
initUDWord i = UDWord (UDIdInt i) "" "" "" "" [] UDIdNone "" "" []

-----------------------
-- printing and parsing
-----------------------

parseUDFile :: FilePath -> IO [UDSentence]
parseUDFile f = readFile f >>= return . parseUDText

parseUDText :: String -> [UDSentence]
parseUDText = map prss . stanzas . lines

-- shorthand
conlluFile2UDTrees :: FilePath -> IO [UDTree]
conlluFile2UDTrees p = parseUDFile p >>= return . map udSentence2tree

errorsInUDSentences :: [UDSentence] -> [String]
errorsInUDSentences = concatMap errors

class Comparable a where
  (=~) :: a -> a -> Bool

instance Comparable UDWord where
  (=~) w x = 
    udFORM w == udFORM x && udUPOS w == udUPOS x && udDEPREL w == udDEPREL x

instance Comparable UDTree where
  (=~) (RTree n ts) (RTree m us) = 
    n =~ m 
    && length ts == length us 
    && and [t =~ u | (t,u) <- ts `zip` us]

class UDObject a where
  prt  :: a -> String  -- print
  prs  :: String -> a  -- parse
  prss :: [String] -> a -- parse from separate lines
  errors :: a -> [String]  -- error messages
  check  :: a -> Either a [String]  -- return a or the error messages 
  (~=) :: a -> a -> Bool
  prss ss = prs (unlines ss)
  prs s = prss [s]
  errors _ = []
  check x = case errors x of [] -> Left x ; ss -> Right (("ERROR in " ++ prt x ++ ": ") : ss)

instance UDObject UDSentence where
  prt s = unlines $ udCommentLines s ++ map prt (udWordLines s)
  prss ss = case span ((=="#") . take 1) ss of
    (cs,ws) -> UDSentence cs (map (prs . strip) ws)
  errors s = checkUDWords (udWordLines s)
  (~=) = undefined 

instance UDObject UDWord where
  prt w@(UDWord id fo le up xp fe he de ds mi) =
    intercalate "\t" [prt id,fo,le,up,xp,prt fe,prt he,de,ds,prt mi]
  prs s = case getSeps '\t' (strip s) of
    id:fo:le:up:xp:fe:he:de:ds:mi:_ ->
      UDWord (prs $ strip id) fo le up xp (prs $ strip fe) (prs $ strip he) de ds (prs $ strip mi) 
    _ -> error ("ERROR: " ++ s ++ " incomplete UDWord")
  errors w@(UDWord id fo le up xp fe he de ds mi) =
    concat [errors id, checkUDPOS up, errors fe, errors he, checkUDLabel de, errors mi] ++
    case w of
      _ | not   ((udHEAD w /= UDIdRoot || udDEPREL w == "root") 
             && (udHEAD w == UDIdRoot || udDEPREL w /= "root"))         -- head 0 iff label root
          -> ["root iff 0 does not hold in:",prt w]
      _ -> []

instance UDObject UDId where
  prt i = case i of
    UDIdInt n -> show n
    UDIdRange m n -> show m ++ "-" ++ show n
    UDIdEmpty f -> show f
    UDIdRoot -> "0"
    UDIdNone -> "_"
  prs s = case (strip s) of
    "0" -> UDIdRoot
    "_" -> UDIdNone
    _ | all isDigit s -> UDIdInt (read s)
    _ -> case break (flip elem ".-") s of
      (a,'-':b@(_:_)) | all isDigit (a++b) -> UDIdRange (read a) (read b)
      (a,'.':b@(_:_)) | all isDigit (a++b) -> UDIdEmpty (read s)
      _ -> error ("ERROR:" ++ s ++ " invalid UDId")
  (~=) = undefined 

instance UDObject UDData where
  prt d = udArg d ++ "=" ++ concat (intersperse "," (udVals d))
  prs s = case break (=='=') (strip s) of
    (a,_:vs@(_:_)) -> UDData a (getSeps ',' vs)
    _ -> error ("ERROR:" ++ s ++ " invalid UDData")
  (~=) = undefined 

--- this works only for | separated lists...
instance UDObject d => UDObject [d] where
  prt ds = case ds of
    [] -> "_" 
    _ -> concat (intersperse "|" (map prt ds))
  prs s = case (strip s) of
    "_" -> []
    _ -> map (prs . strip) (getSeps '|' s)
  errors ds = concatMap errors ds
  (~=) = undefined 

-- printing for Malt parser requires the metadata
-- # sent_id = gfud1000001
-- # text = in the computer
prUDSentence :: Int -> UDSentence -> String
prUDSentence i = prt . addMeta i
 where
   addMeta i u = u {
     udCommentLines = [
       "# sent_id = " ++ show i,
       "# text = " ++ unwords (map udFORM (udWordLines u))
       ]
     }


-- example input: "1 John John NOUN 2 nsubj ; 2 walks walk VERB 0 root"
pQuickUDSentence :: String -> UDSentence
pQuickUDSentence = prss . map completeUDWord . getSeps ";" . words
 where
  completeUDWord ws = case ws of
    index:word:lemma:pos:goal:label:_ -> (concat (intersperse "\t" [index,word,lemma,pos,dum,dum,goal,label,dum,dum]))
    _ -> error $ "no UD word from: " ++ unwords ws
  dum = "_"
    


----------------------------------------------
-- extract word:<pos> sequences, or word:<pos_feats>
----------------------------------------------

ud2poswords :: UDSentence -> String
ud2poswords s = unwords [udFORM u ++ ":<" ++ udUPOS u ++ ">" | u <- udWordLines s]

ud2posfeatswords :: UDSentence -> String
ud2posfeatswords s = unwords [udFORM u ++ ":<" ++ udUPOS u ++ "_" ++ prt (udFEATS u) ++ ">" | u <- udWordLines s]

----------------------------------------------
-- converting to a hierarchical tree and back
----------------------------------------------

type UDTree = RTree UDWord

udSentence2tree :: UDSentence -> UDTree
udSentence2tree s = s2t rootWord where
  s2t hd = RTree hd [s2t w | w <- ws, udHEAD w == udID hd]
  rootWord = head [w | w <- ws, udHEAD w == UDIdRoot] -- unique if check succeeds
  ws = udWordLines s

-- opposite conversion
udTree2sentence :: UDTree -> UDSentence
udTree2sentence t = UDSentence {
  udCommentLines = [],
  udWordLines = sortOn udID (allNodesRTree t)
  }

prUDTree :: UDTree -> String
prUDTree = prLinesRTree prt

-- "prints" the "linearized" UD tree 
prUDTreeString :: UDTree -> String
prUDTreeString t = unwords [udFORM n | n <- sortOn udID (allNodesRTree t)]

--------------------
-- checking for permissible values
--------------------

checkUDWords :: [UDWord] -> [String]
checkUDWords ws = concatMap errors ws ++ case ws of
  _ | length (filter ((==UDIdRoot) . udHEAD) ws) /=1               -- exactly one 0 
        -> ["no unique root in:", pws]
  _ | ids /= [1 .. length ids]
        -> ["word id sequence not 1..n in " ++ pws]
  _ | not (null [i | UDIdInt i <- map udHEAD ws, i > lws || i < 1])
        -> ["head outside sentence in " ++ pws]
  _ -> []
 where
   ids = [n | UDIdInt n <- map udID ws]
   pws = unlines (map prt ws)
   lws = length ids

checkUDPOS :: String -> [String]
checkUDPOS = checkInList "UD Pos tag" (map fst allUDPos)

checkUDLabel :: String -> [String]
checkUDLabel s = let (t,a) = break (==':') s in
  checkInList "UD label" (map fst allUDLabels) t

dep_Label = "dep" -- the dummy label
head_Label = "head"
root_Label = "root"
x_POS = "X" -- the dummy POS

adjustUDIds :: UDSentence -> UDSentence
adjustUDIds uds =
  if ids == [1..length ws]
  then uds
  else uds{udWordLines = map fix ws}
 where
  ws = udWordLines uds
  ids = [n | UDIdInt n <- map udID ws]
  fixes = zip (map udID ws) (map UDIdInt [1..length ws])
  fix udw = udw {
    udID = let idw = udID udw in maybe idw id (lookup idw fixes),
    udHEAD = let idw = udHEAD udw in maybe idw id (lookup idw fixes),
    udDEPS = "ADJUSTED"
    }



---------------------
-- auxiliaries
---------------------

int2udid :: Int -> UDId
int2udid n = case n of
   0 -> UDIdRoot
   _ -> UDIdInt n

udid2int :: UDId -> Int
udid2int i = case i of
   UDIdRange m n -> m ---
   UDIdInt n -> n
   _ -> 0 --- Root, None, Float

nextUDId :: UDId -> UDId
nextUDId ui = int2udid (udPosition ui + 1)

previousUDId :: UDId -> UDId
previousUDId ui = int2udid (udPosition ui - 1)

--- special labels and tags
formData i = [UDData "FORM" [show i]]

numValUDData uddata = case uddata of
  [UDData "FORM" [d]] | all isDigit d -> read d
  _ -> -100 --- junk value not matching any lookup

--- intermediate marking of the spine by prefixing +'s
headLabel l = '+':l
isHeadLabel l = head l == '+'
unHeadLabel l = dropWhile (=='+') l

udPosition udid = case udid of
    UDIdInt i -> i
    UDIdRoot  -> 0
    _ -> error ("ERROR: no position computed from " ++ prt udid) --- never happens in gf2ud...

-- distance between head and dependent
dependencyDistance :: UDWord -> Int
dependencyDistance w = abs (udPosition (udID w) - udPosition (udHEAD w))



checkInList :: String -> [String] -> String -> [String]
checkInList desc xs x = 
  if x=="_" || S.member x xset
  then []
  else ["invalid " ++ desc ++ ": " ++ x]
 where
   xset = S.fromList xs

getSeps :: Eq a => a -> [a] -> [[a]]
getSeps p xs = filter (not .null) getSeps'
  where
    getSeps' = case break (==p) xs of
      (c,_:xx) -> c : getSeps p xx
      (c,_) -> [c]

stanzas :: [String] -> [[String]]
stanzas ls = case dropWhile (all isSpace) ls of
  []  -> []
  wls -> case break (all isSpace) wls of
    (s,ss) -> s : stanzas ss

strip :: String -> String
strip [] = []
strip (c:cs)
  | isSpace c = strip cs
  | otherwise = reverse $ strip' (reverse $ c:cs)
  where
    strip' [] = []
    strip' (c:cs)
      | isSpace c = strip' cs
      | otherwise = (c:cs)
------------------
-- evaluations
-----------------

-- labelled attachment score

data UDScore = UDScore {
  udScore          :: Double, --- redundant
  udMatching       :: Int, -- if the words are the same. 1 or 0 for a single sentence, sum for a corpus
  udTotalLength    :: Int, -- number of words
  udSamesLength    :: Int, -- number of words with matching (head,label)
  udPerfectMatch   :: Int  -- all words have match (head,label). 1 or 0 for single sentence, sum for corpus
  }
 deriving Show

type ScoringCriterion = UDWord -> UDWord -> Bool

agreeLAS g t = udHEAD g == udHEAD t && udDEPREL g == udDEPREL t
agreeUAS g t = udHEAD g == udHEAD t 

-- return the best candidate and its score
udSentenceScore :: ScoringCriterion -> UDSentence -> [UDSentence] -> (UDSentence,UDScore)
udSentenceScore agree gold testeds = (tested,score) where

  score = UDScore {
    udScore = fromIntegral (length sames) / fromIntegral (length alls),
    udMatching = areMatching,
    udTotalLength = length alls,
    udSamesLength = length sames,
    udPerfectMatch = if (length sames == length alls) then 1 else 0
    } 

  alls = udWordLines tested
  tested = maximumBy (\t u -> compare (length (samest t)) (length (samest u))) testeds
  sames = samest tested
  samest tsd = [() | (g,t) <- zip (udWordLines gold) (udWordLines tsd), agree g t]
  areMatching = if (map udFORM (udWordLines gold) == map udFORM (udWordLines tested)) then 1 else 0


-- test on corpus level: in the tested corpus, group together trees for the same sentence
udCorpusScore :: Bool -> ScoringCriterion -> [UDSentence] -> [UDSentence] -> UDScore
udCorpusScore isMicro agree golds tests = UDScore {
  udScore = if isMicro
            then fromIntegral numbersames / fromIntegral numberalls                -- micro score (per word)
            else sum (map udScore sentenceScores) / fromIntegral (length sentenceScores), -- macro score (per sentence)
  udMatching = sum (map udMatching sentenceScores),
  udTotalLength = numberalls,
  udSamesLength = numbersames,
  udPerfectMatch = sum (map udPerfectMatch sentenceScores)
  }
  where
    sentenceScores =
      filter ((>0) . udMatching) $ map snd $
        map (uncurry (udSentenceScore agree)) (zip golds testgroups)
    numberalls  =  sum (map udTotalLength sentenceScores)
    numbersames =  sum (map udSamesLength sentenceScores)
    testgroups  = groupBy (\t u -> sent t == sent u) tests
    sent t = unwords $ map udFORM $ udWordLines t

---------------------------
-- transforming UD trees --
---------------------------

rewriteUDTree :: UDTree -> UDTree
rewriteUDTree udt = udt
---- TODO