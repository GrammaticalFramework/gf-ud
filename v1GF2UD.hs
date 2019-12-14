module GF2UD where

import UDConcepts
import PGF hiding (CncLabels)

import qualified Data.Map as M
import Data.List
import Data.Char

---------
-- to test

-- env <- getEnv

getEnv = do
  pgf <- readPGF "Lang.pgf"
  abslabels <- readFile "Lang.labels" >>= return . pAbsLabels
  cnclabels <- readFile "LangEng.labels" >>= return . const . pCncLabels
  putStrLn $ unlines $ checkAbsLabels pgf abslabels
  return $ mkEnv pgf abslabels cnclabels

test env s = do
  let t = parseEng env s
  putStrLn $ showExpr [] t
  let e = expr2annottree env eng t
  putStrLn $ prLinesRTree prAnnotNode e
  let u = annottree2udsentence e
  putStrLn (prt u)
  putStrLn $ unlines (errors u)

pTree s = maybe undefined id $ readExpr s
eng = maybe undefined id $ readLanguage "LangEng"
parseEng env s = head $ parse (pgfGrammar env) eng (maybe undefined id $ readType "S") s
mkEnv pgf absl cncl = initUDEnv {pgfGrammar = pgf, absLabels = absl, cncLabels = cncl}

--------

data UDEnv = UDEnv {
  udFormat   :: String,
  absLabels  :: AbsLabels,
  cncLabels  :: Language -> CncLabels,
  pgfGrammar :: PGF
  }

initUDEnv = UDEnv "conllu" (AbsLabels M.empty M.empty) (const initCncLabels) (error "no pgf")

data AbsLabels = AbsLabels {
  funLabels :: M.Map CId [String],
  catLabels :: M.Map CId String
  }

-- is be VERB cop head
data CncLabels = CncLabels {
  wordLabels    :: M.Map String (String,String,[UDData]),  -- word -> (lemma,pos,morpho)          e.g. #word been be AUX  Tense=Past|VerbForm=Part
  lemmaLabels   :: M.Map (CId,String) (String,String),     -- (fun,lemma) -> (label,targetLabel), e.g. #lemma UseComp be cop head
  morphoLabels  :: M.Map (CId,Int) [UDData],               -- (cat,int) -> morphotag,             e.g. #morpho V,V2,VS 0 VerbForm=Inf
  discontLabels :: M.Map (CId,Int) (String,String,String)  -- (cat,field) -> (pos,label,target)   e.g. #discont  V2  5,ADP,case,obj   6,ADV,advmod,head
  }

initCncLabels = CncLabels M.empty M.empty M.empty M.empty

-- check the soundness of labels

checkAbsLabels :: PGF -> AbsLabels -> [String]
checkAbsLabels pgf als =
  ---- check completeness, too, as well as if a function is included twice
  concatMap chFun funs ++
  concatMap chCat cats 
 where
  funs = M.toList (funLabels als)
  cats = M.toList (catLabels als)

  chFun (f,ls) =
    ["unknown function " ++ showCId f | notElem f (functions pgf)] ++
    ["no head in "       ++ showCId f | notElem "head" ls] ++
    ["wrong number of labels in " ++ showCId f ++ " : " ++ showType [] typ |
      Just typ <- [functionType pgf f],
      let (hs,_,_) = unType typ,
      length hs /= length ls
      ] ++
    concatMap checkUDLabel (filter (/="head") ls)

  chCat (c,p) =
    ["unknown category " ++ showCId c | notElem c (categories pgf)] ++
    checkUDPOS p


-- get the labels from file

pAbsLabels :: String -> AbsLabels
pAbsLabels = build . dispatch . map words . uncomment . lines
 where
  dispatch = partition (\ws -> length ws > 2 || last ws == head_Label)
  build (fs,cs) = AbsLabels {
    funLabels = M.fromList [(mkCId f,ls) | f:ls  <- fs],
    catLabels = M.fromList [(mkCId c,p)  | c:[p] <- cs]
    }

pCncLabels :: String -> CncLabels
pCncLabels = dispatch . map words . uncomment . lines
 where
  dispatch = foldr add initCncLabels
  add ws labs = case ws of
    "#morpho"  :cs:i:p:_  | all isDigit i -> labs{morphoLabels = inserts [((mkCId c,read i),(prs p)::[UDData]) | c <- getSeps ',' cs] (morphoLabels labs)}
    "#word"    :w:l:p:m:_ -> labs{wordLabels   = M.insert w (l,p,prs m) (wordLabels labs)}
    "#lemma"   :w:l:p:t:_ -> labs{lemmaLabels  = inserts [((mkCId f,l),(p,t)) | f <- getSeps ',' w] (lemmaLabels labs)}
    "#discont" :c:ps      -> labs{discontLabels = inserts
                                [((mkCId c,read i),(pos,lab,hd)) | p <- ps, let i:pos:lab:hd:_ = getSeps ',' p] (discontLabels labs)}

    _ -> labs --- ignores silently

  inserts kvs mp = foldr (\ (k,v) m -> M.insert k v m) mp kvs 

uncomment :: [String] -> [String]
uncomment = filter (not . all isSpace)  . map uncom
 where
  uncom cs = case cs of
    '-':'-':_ -> ""
    c:cc -> c:uncom cc
    _ -> cs
    
    

-- standard GF abstract tree built from constructors
type AbsTree = RTree CId 

expr2abstree :: PGF.Expr -> AbsTree
expr2abstree e = case unApp e of
  Just (f,es) -> RTree f (map expr2abstree es)
  _ -> error ("ERROR: no constructor tree from " ++ showExpr [] e)

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

-- richly annotated abstract tree
type AnnotTree = RTree AnnotNode

data AnnotNode = AnnotNode {
  anFun    :: CId,
  anCat    :: CId,
  anLabel  :: String, -- UD label coming from above
  anHead   :: Int, -- the head leaf in this subtree (not the same as head in UD)
  anUDHead :: Int, -- the UD head, outside this subtree
  anTokens :: [(Int,TokenInfo)] -- tokens dominated by this node, with position number
  }
 deriving Show

---  anTokens :: [(Int,(String,String,String,[UDData],String))] -- tokens dominated by this node
data TokenInfo = TokenInfo {
  tokWord   :: String,
  tokLemma  :: String,
  tokPOS    :: String,
  tokMorpho :: [UDData],
  tokLabel  :: String, -- needed for syncat words
  tokHead   :: Int     -- needed for syncat words
  }
 deriving Show
 
mainPositionAnnotNode :: AnnotNode -> Int
mainPositionAnnotNode n = case anTokens n of
  (i,_):_ -> i ---- first word of a multiword
  _ -> 0 ----

prAnnotNode (AnnotNode f c l h u ts) =
  unwords [showCId f,showCId c,l,show h++"->"++show u,
           unwords (intersperse ";" [show i ++ ": " ++ prTokenInfo t | (i,t) <- ts])
           ]
prTokenInfo (TokenInfo w l p m lab hd) = unwords [w,l,p,prt m,lab,show hd]

------------------
-- the final part of ud2gf

annottree2udsentence :: AnnotTree -> UDSentence
annottree2udsentence tr = UDSentence [] (sortOn (udPosition . udID) wls) where
  wls = [UDWord {
           udID     = UDIdInt position,
           udFORM   = tokWord tok,
           udLEMMA  = tokLemma tok,
           udUPOS   = tokPOS tok,
           udXPOS   = showCId (anCat n),
           udFEATS  = tokMorpho tok,
           udDEPREL = if (isSyncat tok) then (tokLabel tok) else mainlabel n,
           udHEAD   = int2udid (if (isSyncat tok) then (tokHead tok) else anUDHead n),
           udDEPS   = "_",
           udMISC   = [UDData "FUN" [showCId (anFun n)]] ----
           } |
               n <- allNodesRTree tr,
               (position,tok) <- anTokens n
       ]
  mainlabel n = unHeadLabel (anLabel n) 
  isSyncat tok = tokLabel tok /= dep_Label ---- not quite the truth, TODO

expr2annottree :: UDEnv -> Language -> Tree -> AnnotTree
expr2annottree env lang tree =
    applyCnc
  $ markLeaves 0
  $ headsUp
  $ addLabels root_Label
  $ mapRTree addWordsAndCats
  $ postOrderRTree
  $ expr2abstree
    tree
  
 where
   pgf = pgfGrammar env
   lookFun f = maybe []    id (M.lookup f (funLabels (absLabels env)))
   lookCat c = maybe x_POS id (M.lookup c (catLabels (absLabels env)))
   lookWord d w = maybe d id (M.lookup w (wordLabels (cncLabels env lang)))
   lookLemma d f w = maybe d id (M.lookup (f,w) (lemmaLabels (cncLabels env lang))) -- (fun,lemma) -> (label,targetLabel)
   lookMorpho d c i = maybe d id (M.lookup (c,i) (morphoLabels (cncLabels env lang))) -- i'th form of cat c
   lookupDiscont c i = (M.lookup (c,i) (discontLabels (cncLabels env lang))) -- (cat,field) -> (pos,label,target)

   applyCnc tr@(RTree node trs) = case trs of
     _:_ -> RTree (node{anTokens = [(i,apptok tok) | (i,tok) <- anTokens node]}) (map applyCnc trs)
     _ -> case anTokens node of
       toks@(_:_) -> RTree (node{anTokens = [(i,appdiscont toks tok) | (i,tok) <- toks]}) (map applyCnc trs)
       _ -> tr
    where
     appdiscont toks tok = case lookupDiscont (anCat node) (numValUDData (tokMorpho tok)) of
       Just (pos,lab,hd) -> tok {tokLabel = lab, tokHead = headPosition toks, tokPOS = pos, tokLemma = tokWord tok}
       _ -> tok

     ---- link to the first non-annotated sister token, thus always to the head. Works for "look up" but not "look for"
     headPosition toks = head [i | (i,t) <- toks, lookupDiscont (anCat node) (numValUDData (tokMorpho t)) == Nothing]

     apptok tok = tok {tokLabel = lab, tokHead = findSibling hd}
      where
       (lab,hd) = lookLemma (tokLabel tok, head_Label) (anFun node) (tokLemma tok)
       
     findSibling hd = case [anHead n | t@(RTree n _) <- trs, unHeadLabel (anLabel n) == hd] of
         i:_ -> i
         _ -> anHead node --- silently links to head if the requested label is not found

   -- propagate UD head links down to leaves; for syncat words, use the GF head of the node and the dummy syncat label
   markLeaves i tr@(RTree node trs) = case trs of
     [] -> RTree (node{anUDHead=i}) []
     _  -> RTree (node{anUDHead=anHead node}) (map (higherHead i (anHead node)) trs)
    where
      higherHead i j t = if anHead (root t) == j then markLeaves i t else markLeaves j t

   -- propagate phrase head links up to the AST nodes that they are the head words of
   headsUp tr@(RTree node trs) = RTree (node{anHead = mainPositionAnnotNode (headLeaf tr)}) (map headsUp trs)
    where
     headLeaf tr = case subtrees tr of
       []  -> root tr
       trs -> headLeaf $ head [t | t <- trs, isHeadLabel (anLabel (root t))]

   -- add abslabels from function annotations, propagate them down the spines but keep marking with + when going down
   addLabels label tr@(RTree node trs) =
     let ls = lookFun (anFun node)
     in RTree (node{anLabel = label}) [addLabels (if lab==head_Label then (headLabel label) else lab) t | (lab,t) <- zip ls trs]

   -- convert postorder GF to AnnotTree by adding words from bracketed linearization, categories and their UD pos tags, morpho indices
   addWordsAndCats (f,i) = AnnotNode {
     anFun = f,
     anCat = cat,
     anHead = 0,
     anUDHead = 0,
     anTokens = maybe [] (map (addLemma f)) (M.lookup i positions),  --- empty list just because lookup fails ??
     anLabel = dep_Label
     }
    where
     (cat,isLeaf) = valCat f

     addLemma f (posit,(w,lind)) = case isLeaf of
         True -> (posit,TokenInfo w (mkLemma f) (lookCat cat) (lookMorpho (formData lind) cat lind) dep_Label 0)  -- lexical word
         _ -> case lookWord (w,x_POS,[]) w of
           (lemma,postag,morph) -> (posit,TokenInfo w lemma postag morph dep_Label 0)     -- syncat word

     mkLemma f = unwords $ take 1 $ words $ linearize pgf lang (mkApp f []) --- if multiword, 1st word is lemma; e.g. "listen to"
   
     positions = bracketPositions $ case bracketedLinearize pgf lang tree of
       b:_ -> b
       _ -> error ("ERROR: no linearization for tree " ++ showExpr [] tree)
     
   valCat f = case functionType pgf f of
     Just typ -> case unType typ of
       (hs,cat,_) -> (cat, null hs)  -- valcat, whether atomic
     _ -> error ("ERROR: cannot find type of function " ++ showCId f)

-- for each in-order abs-node, find the words that the node dominates, with their position and morpho
bracketPositions :: BracketedString -> M.Map FId [(Int,(String,LIndex))]
bracketPositions = collect . numerate . pos 0 0

 where
 
   pos fid lindex bs = case bs of
     Bracket _ fi _ lind _ _ bss -> concatMap (pos fi lind) bss
     Leaf s -> [(fid,(w,lindex)) | w <- words s] -- separate words --- every word gets the same morpho index

   numerate fws = [(f,(p,wl)) | (p,(f,wl)) <- zip [1..] fws] -- assign a position number to every word
     
   collect fpws = M.fromListWith (++) [(f,[pwl])  | (f,pwl) <- fpws]  -- map abstree nodes to word information

---------------------
-- auxiliaries
---------------------

int2udid :: Int -> UDId
int2udid n = case n of
   0 -> UDIdRoot
   _ -> UDIdInt n

--- special labels and tags
syncat_Label = "syncat"
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

