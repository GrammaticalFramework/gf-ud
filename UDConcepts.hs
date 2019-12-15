module UDConcepts where

-- AR 2019-11-14 implementing
-- https://universaldependencies.org/format.html

import GFConcepts
import UDStandard

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
  }

type POS = String
type Label = String

data UDData = UDData {
  udArg  :: String,
  udVals ::[String]
  } -- Arg=Val,Val,Val
   deriving (Eq,Show)

initUDWord :: Int -> UDWord
initUDWord i = UDWord (UDIdInt i) "" "" "" "" [] UDIdNone "" "" []

-----------------------
-- printing and parsing
-----------------------

parseUDFile :: FilePath -> IO [UDSentence]
parseUDFile f = readFile f >>= return . map prss . stanzas . lines

errorsInUDSentences :: [UDSentence] -> [String]
errorsInUDSentences = concatMap errors

class UDObject a where
  prt  :: a -> String  -- print
  prs  :: String -> a  -- parse
  prss :: [String] -> a -- parse from separate lines
  errors :: a -> [String]  -- error messages
  check  :: a -> Either a [String]  -- return a or the error messages 
  prss ss = prs (unlines ss)
  prs s = prss [s]
  errors _ = []
  check x = case errors x of [] -> Left x ; ss -> Right (("ERROR in " ++ prt x ++ ": ") : ss)

instance UDObject UDSentence where
  prt s = unlines $ udCommentLines s ++ map prt (udWordLines s)
  prss ss = case span ((=="#") . take 1) ss of
    (cs,ws) -> UDSentence cs (map prs ws)
  errors s = checkUDWords (udWordLines s)

instance UDObject UDWord where
  prt w@(UDWord id fo le up xp fe he de ds mi) =
    concat (intersperse "\t" [prt id,fo,le,up,xp,prt fe,prt he,de,ds,prt mi])
  prs s = case getSeps '\t' s of
    id:fo:le:up:xp:fe:he:de:ds:mi:_ ->
      UDWord (prs id) fo le up xp (prs fe) (prs he) de ds (prs mi)
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
  prs s = case s of
    "0" -> UDIdRoot
    "_" -> UDIdNone
    _ | all isDigit s -> UDIdInt (read s)
    _ -> case break (flip elem ".-") s of
      (a,'-':b@(_:_)) | all isDigit (a++b) -> UDIdRange (read a) (read b)
      (a,'.':b@(_:_)) | all isDigit (a++b) -> UDIdEmpty (read s)
      _ -> error ("ERROR:" ++ s ++ " invalid UDId")

instance UDObject UDData where
  prt d = udArg d ++ "=" ++ concat (intersperse "," (udVals d))
  prs s = case break (=='=') s of
    (a,_:vs@(_:_)) -> UDData a (getSeps ',' vs)
    _ -> error ("ERROR:" ++ s ++ " invalid UDData")

--- this works only for | separated lists...
instance UDObject d => UDObject [d] where
  prt ds = case ds of
    [] -> "_" 
    _ -> concat (intersperse "|" (map prt ds))
  prs s = case s of
    "_" -> []
    _ -> map prs (getSeps '|' s)
  errors ds = concatMap errors ds


-- example input: "1 John John NOUN 2 nsubj ; 2 walks walk VERB 0 root"
pQuickUDSentence :: String -> UDSentence
pQuickUDSentence = prss . map completeUDWord . getSeps ";" . words
 where
  completeUDWord ws = case ws of
    index:word:lemma:pos:goal:label:_ -> (concat (intersperse "\t" [index,word,lemma,pos,dum,dum,goal,label,dum,dum]))
    _ -> error $ "no UD word from: " ++ unwords ws
  dum = "_"
    

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


---------------------
-- auxiliaries
---------------------

int2udid :: Int -> UDId
int2udid n = case n of
   0 -> UDIdRoot
   _ -> UDIdInt n

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



checkInList :: String -> [String] -> String -> [String]
checkInList desc xs x = 
  if x=="_" || S.member x xset
  then []
  else ["invalid " ++ desc ++ ": " ++ x]
 where
   xset = S.fromList xs

getSeps :: Eq a => a -> [a] -> [[a]]
getSeps p xs = case break (==p) xs of
  (c,_:xx) -> c : getSeps p xx
  (c,_) -> [c]

stanzas :: [String] -> [[String]]
stanzas ls = case dropWhile (all isSpace) ls of
  []  -> []
  wls -> case break (all isSpace) wls of
    (s,ss) -> s : stanzas ss


