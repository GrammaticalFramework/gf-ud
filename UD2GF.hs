module UD2GF where

import UDConcepts
import UDAnnotations
import GFConcepts
import UDOptions

import PGF hiding (CncLabels)

import qualified Data.Map as M
import Data.List
import Data.Char
import Data.Maybe

import Debug.Trace (trace)

---------
-- to debug

tracePrint p m x = trace (m ++ " : " ++ p x) x
----traceShow = tracePrint show
traceNoPrint _ _ x = x

-- to test

-- env <- getEnv


test opts env string = do
  let eng = actLanguage env
  let sentences = map prss $ stanzas $ lines string
  tstats <- mapM (showUD2GF opts env) sentences
  let globalStats = combineUD2GFStats $ map snd tstats
  ifOpt opts "stat" $ prUD2GFStat globalStats
  return ()

showUD2GF opts env sentence = do
  
  ifOpt opts "ud" $ prt sentence

  case errors sentence of
    [] -> return ()
    errs -> ifOpt opts "err" $ unlines errs
 
  let udtree = udSentence2tree sentence
  ifOpt opts "ut" $ prUDTree udtree

  let devtree0 = udtree2devtree udtree
  ifOpt opts "dt0" $ prLinesRTree (prDevNode 2) devtree0

  let devtree1 = analyseWords env devtree0
  ifOpt opts "dt1" $ prLinesRTree (prDevNode 2) devtree1
  
  let devtree = combineTrees env devtree1
  ifOpt opts "dt" $ prLinesRTree (prDevNode 4) devtree

  let besttree0 = head (splitDevTree devtree)
  ifOpt opts "bt0" $ prLinesRTree (prDevNode 1) besttree0

  let besttree = addBackups besttree0
  ifOpt opts "bt" $ prLinesRTree (prDevNode 1) besttree
  
  let ts0 = devtree2abstrees besttree
  ifOpt opts "at0" $ unlines $ map prAbsTree ts0

  let ts = map (expandMacro env) ts0
  ifOpt opts "at" $ unlines $ map prAbsTree ts

  let allnodes = allNodesRTree besttree0
      orig = length allnodes
      interp = length (devStatus (root besttree0))
      stat = UD2GFStat {
       totalWords = orig,
       interpretedWords = interp,
       unknownWords = length [dn | dn <- allnodes, devIsUnknown dn],
       totalSentences = 1,
       completeSentences = div interp orig -- either 1 or 0
       }
  
  return (ts,stat)


data UD2GFStat = UD2GFStat {
  totalWords :: Int,
  interpretedWords :: Int,
  unknownWords :: Int,
  totalSentences :: Int,
  completeSentences :: Int
  }
 deriving Show

prUD2GFStat :: UD2GFStat -> String
prUD2GFStat stat = unlines $ [
  "total word nodes:\t"                    ++ show (totalWords stat),
  "interpreted word nodes:\t"              ++ show (interpretedWords stat) ++ proportion interpretedWords totalWords,
  "unknown word nodes (tokens):\t"         ++ show (unknownWords stat) ++ proportion unknownWords totalWords,
  "total sentences:\t"                     ++ show (totalSentences stat),
  "completely interpreted sentences:\t"    ++ show (completeSentences stat) ++ proportion completeSentences totalSentences
  ]
 where
   proportion f g = " (" ++ show (div (100 * f stat) (g stat)) ++ "%)"

combineUD2GFStats :: [UD2GFStat] -> UD2GFStat
combineUD2GFStats stats = UD2GFStat {
  totalWords = sum (map totalWords stats),
  interpretedWords = sum (map interpretedWords stats),
  unknownWords = sum (map unknownWords stats),
  totalSentences = sum (map totalSentences stats),
  completeSentences = sum (map completeSentences stats)
  }

{-
-- the pipeline
ud2gf :: UDEnv -> UDTree -> [PGF.Expr]
ud2gf env =
    map abstree2expr
  . devtree2abstrees
  . transformDevTree env
  . udtree2devtree
-}

-- developing tree on the way from UD to GF
type DevTree = RTree DevNode
data DevNode = DevNode {
  devStatus     :: [UDId],         -- indices of words used in the best abstree in DevTrees --- redundant
  devWord       :: String,         -- the original word
  devAbsTrees   :: [AbsTreeInfo],  -- trees constructed at this node, with types and used words
  devLemma      :: String, 
  devPOS        :: String,
  devFeats      :: [UDData],
  devLabel      :: String,
  devIndex      :: UDId,   -- position in the original sentence
  devNeedBackup :: Bool,   -- if this node needs to be covered by Backup
  devIsUnknown  :: Bool    -- if the word at this node is unknown
 }
  deriving Show

type AbsTreeInfo = (AbsTree,(Cat,[UDId]))

-- n shows how many trees are to be shown
prDevNode n d = unwords [
  (if (devNeedBackup d) then "*" else "") ++
    prtStatus (devStatus d),
  devWord d,
  prt (devIndex d),
  devPOS d,
  devLabel d,
  "(" ++ unwords (intersperse ";"
    [prAbsTree e ++ " : " ++ showCId c ++ prtStatus us | (e,(c,us)) <- take n (devAbsTrees d)]) ++ ")",
  show (length (devAbsTrees d))
  ]

devtree2abstrees :: DevTree -> [AbsTree]
devtree2abstrees = map fst . devAbsTrees . root

-- to be applied to a DevTree with just one tree at each node
addBackups :: DevTree -> DevTree
addBackups tr@(RTree dn trs) = case map collectBackup (tr:trs) of
  btrs -> RTree (dn{devAbsTrees = [replaceInfo [(t,ai) | (_,(t,Just ai)) <- btrs] (theAbsTreeInfo tr)]}) (map fst (tail btrs))
  
 where

  replaceInfo :: [(AbsTree,AbsTreeInfo)] -> AbsTreeInfo -> AbsTreeInfo
  replaceInfo btrs ai@(ast,(cat,usage)) =
    (replace btrs ast,(cat,sort (nub (concat (usage:map (snd . snd . snd) btrs)))))

  replace :: [(AbsTree,AbsTreeInfo)] -> AbsTree -> AbsTree
  replace btrs tr@(RTree f trs) = case lookup tr btrs of
    Just (btr,(c,_)) -> appBackup c btr tr
    _ -> RTree f (map (replace btrs) trs)

  collectBackup :: DevTree -> (DevTree,(AbsTree,Maybe AbsTreeInfo))
  collectBackup t@(RTree d ts) =
    let ai@(ast,_) = theAbsTreeInfo t in
    (t,(ast, mkBackupList ai [theAbsTreeInfo (addBackups u) | u <- ts, devNeedBackup (root u)]))

  mkBackupList :: AbsTreeInfo -> [AbsTreeInfo] -> Maybe AbsTreeInfo
  mkBackupList ai@(ast,(cat,usage)) ts =
    case unzip [(mkBackup a c,us) | (a,(c,us)) <- ts] of
      ([],_) -> Nothing
      (bs,uss) -> Just (foldr cons nil bs, (cat,sort (nub (concat uss))))

  mkBackup ast cat = RTree (mkCId (showCId cat ++ "Backup")) [ast]

  cons t u = RTree (mkCId "ConsBackup") [t,u]
  nil = RTree (mkCId "BaseBackup") []

  appBackup :: Cat -> AbsTree -> AbsTree -> AbsTree
  appBackup cat b t = RTree (mkCId ("AddBackup" ++ showCId cat)) [b,t]


-- call this to make sure that the abs tree info is unique
theAbsTreeInfo :: DevTree -> AbsTreeInfo
theAbsTreeInfo dt = case devAbsTrees (root dt) of
  [t] -> t
  _ -> error $ "no unique abstree in " ++ prDevNode 2 (root dt)

-- split trees showing just one GF tree in each DevTree
splitDevTree :: DevTree -> [DevTree]
splitDevTree tr@(RTree dn trs) =
  [RTree (dn{devAbsTrees = [t]}) (map (chase t) trs) | t <- devAbsTrees dn]
 where
  chase (ast,(cat,usage)) tr@(RTree d ts) = case elem (devIndex d) usage of
    True -> case sortOn ((1000-) . sizeRTree . fst) [dt | dt@(t,_) <- devAbsTrees d, isSubRTree t ast] of
      t:_ -> RTree (d{devAbsTrees = [t]}) (map (chase t) ts)
      _ -> error $ "wrong indexing in\n" ++ prLinesRTree (prDevNode 1) tr
    False -> head $ splitDevTree $ RTree (d{devNeedBackup = True}) ts ---- head

prtStatus udids =  "[" ++ concat (intersperse "," (map prt udids)) ++ "]"


-- order collected abstract trees by completeness; applied internally in combineTree at each node
rankDevTree :: DevTree -> DevTree
rankDevTree tr@(RTree dn dts) = RTree dn{devAbsTrees = rankSort (devAbsTrees dn)} dts
 where
  rankSort = sortOn ((100-) . rank) -- descending order of rank
  rank (t,(c,us)) = length us

-- omit (t2,(cat,usage2)) if there is (t1,(cat,usage1)) such that usage2 is a subset of usage1
pruneDevTree :: DevTree -> DevTree
pruneDevTree  tr@(RTree dn dts) = RTree dn{devAbsTrees = pruneCatGroups (groupCat (devAbsTrees dn))} dts
 where
  cat = fst . snd
  usage = snd . snd
  rank = length . usage
  groupCat = map (sortOn ((100-) . rank)) . groupBy (\x y -> cat x == cat y) . sortOn cat
  prune usages grp = case grp of
    t:ts | any (\u -> all (\x -> elem x u) (usage t)) usages -> prune usages ts
    t:ts -> t : prune (usage t : usages) ts
    _ -> grp  
  pruneCatGroups = concatMap (prune [])
  
data FunInfo = FunInfo {
  funFun   :: Fun,
  funTyp   :: LabelledType,
  funTree  :: AbsTree,
  funUsage :: [UDId]
  }

data ArgInfo = ArgInfo {
  argNumber :: Int,
  argUsage  :: [UDId],
  argCatLab :: (Cat,Label),
  argTree   :: AbsTree
  }

combineTrees :: UDEnv -> DevTree -> DevTree
combineTrees env =
     rankDevTree
   . comb

 where

  comb :: DevTree -> DevTree
  comb tr@(RTree dn dts) = case map comb dts of
    ts -> keepTrying (RTree dn ts)

  keepTrying :: DevTree -> DevTree
  keepTrying tr = case tryCombine (allFunsLocal tr)  tr of
    tr' | devAbsTrees (root tr') /= devAbsTrees (root tr) -> keepTrying tr'
    _ -> traceNoPrint (prDevNode 3 . root) "built" $ pruneDevTree $ rankDevTree tr

  tryCombine :: [FunInfo] -> DevTree -> DevTree
  tryCombine finfos tr = case finfos of
    f:fs -> tryCombine fs (addAbsTree f tr)
    [] -> tr

  allFunsLocal :: DevTree -> [FunInfo]
  allFunsLocal tr@(RTree dn ts) =
    [FunInfo f labtyp abstree usage |
        (f,labtyp) <- allFunsEnv,

        -- for head and each immediate subtree, build the list of its already built abstrees, each with type and label
        -- argalts :: [[Arg]] -- one list for root and for each subtree
        let argalts = [
                       [ArgInfo i us (c, devLabel r) e | (e,(c,us)) <- devAbsTrees r]
                           |
                             (i,r) <- (0,dn{devLabel = head_Label}) :  -- number the arguments: root node 0, args 1,..
                                                 [(i,r) | (i,r) <- zip [1..] (map root ts)]
                     ],

        -- argument sequences: an argument whose index is already in [Int] may not be used
        -- argseqsAfter :: [Int] -> [[Arg]] -> [[Arg]]
        let argseqsAfter us argss =
              [filter (\x -> all (flip notElem us) (argUsage x)) xs  | xs <- sequence argss],

        let argseqs (arg:args) = [x:xs | x <- arg, xs <- argseqsAfter (argUsage x) args],
        
        (abstree,usage) <- tryFindArgs f labtyp (argseqs argalts)
      ]

  tryFindArgs :: CId -> LabelledType -> [[ArgInfo]] -> [(AbsTree,[UDId])]
  tryFindArgs f labtyp@(valcat,catlabs) argss =
    [(abstree,usage) |
        args <- argss,
        xis  <- (argTypeMatches catlabs args),
        let abstree = RTree f (map fst xis),
        let usage = sort (nub (concatMap argUsage (take 1 args) ++ concatMap snd xis)) -- head usage + dependents' argument numbers
    ]

  argTypeMatches :: [(Cat,Label)] -> [ArgInfo] -> [[(AbsTree,[UDId])]]
  argTypeMatches catlabs args = case catlabs of
    catlab1:catlabs2 -> [
      [(t,i) | (t,i) <- (argTree x, argUsage x):xs]
        |
          x  <- [arg | arg <- args, argCatLab arg == catlab1],
          xs <- argTypeMatches catlabs2 [arg | arg <- args, argNumber arg /= argNumber x]
      ]
    _ -> [[]]

  addAbsTree :: FunInfo -> DevTree -> DevTree
  addAbsTree finfo tr@(RTree dn ts) =
    RTree dn{
      devAbsTrees = let
                   acu = (funTree finfo,(fst (funTyp finfo),funUsage finfo))
                   dts = devAbsTrees dn
                 in
                 if elem acu dts  -- the same tree with the same usage of subtrees is not added again
                    || length dts > 10 ---- debug
                   then dts
                   else acu:dts,
      devStatus = maximumBy (\x y -> compare (length x) (length y)) [devStatus dn, funUsage finfo]
      } ts

  -- macros + real abstract functions
  allFunsEnv =
    [(f,(val,zip args ls))  |
      (f,((val,args),((xx,df),ls))) <- M.assocs (macroFunctions (absLabels env))]
     ++
    [(f, mkLabelledType typ labels) |
      (f,(labels,True)) <- M.assocs (funLabels (absLabels env)),
      Just typ   <- [functionType (pgfGrammar env) f]
    ]
     ++
    [(f, mkLabelledType typ labels) |
      (f,labelss) <- M.assocs (altFunLabels (absLabels env)),
      labels      <- labelss,
      Just typ    <- [functionType (pgfGrammar env) f]
    ]


analyseWords :: UDEnv -> DevTree -> DevTree
analyseWords env = mapRTree lemma2fun
 where
  lemma2fun dn = dn {
    devAbsTrees = [(t,(c,[devIndex dn])) | (t,c) <- justWords],
    devStatus = [devIndex dn],
    devIsUnknown = isUnknown
    }
   where
    (isUnknown,justWords) = getWordTrees (devLemma dn) (cats (devPOS dn))
    
  cats pos = maybe [] id $ M.lookup pos (catsForPOS env)

  -- find all functions that are possible parses of the word in any appropriate category
  --- it is still possible that some other category is meant
  getWordTrees w cs = case concatMap (parseWord w) cs of
    [] -> case cs of
      []  -> (True,[(newWordTree w unknownCat, unknownCat)])
      _ -> (True,[(newWordTree w c, c) | c <- cs])

    fs -> (False,fs)

  --- this can fail if c is discontinuous, or return false positives if w is a form of another word
  parseWord w c = case parse (pgfGrammar env) (actLanguage env) (mkType [] c []) w of
    ts -> [(expr2abstree t,c) | t <- ts]

  newWordTree w c = RTree (mkCId (w ++ "_x_" ++ showCId c)) []
  ---newWordTree w c = RTree (mkCId ("MkNew"++showCId c)) [RTree (mkCId (quote w)) []]

  unknownCat = mkCId "Adv" --- treat unknown words as adverbs ---- to be parameterized

  quote s = "\"" ++ s ++ "\""

udtree2devtree :: UDTree -> DevTree
udtree2devtree tr@(RTree un uts) =
  RTree (DevNode {
      devStatus = [],
      devWord  = udFORM un,
      devAbsTrees = [],
      devLemma = udLEMMA un,
      devPOS   = udUPOS un,
      devFeats = udFEATS un,
      devLabel = udDEPREL un,
      devIndex = udID un,
      devNeedBackup = False,
      devIsUnknown = True
     }) (map udtree2devtree uts)
