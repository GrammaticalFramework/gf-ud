module UD2GF where

import UDConcepts
import UDAnnotations
import GFConcepts
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

test env file = do
  let eng = actLanguage env
  sentences <- parseUDFile file
  mapM (showUD2GF env) sentences
  return ()

showUD2GF env sentence = do
  putStrLn (prt sentence)
  putStrLn $ unlines (errors sentence)
  
  let udtree = udSentence2tree sentence
  putStrLn $ prUDTree udtree

  let devtree0 = udtree2devtree udtree
  putStrLn $ prLinesRTree (prDevNode 2) devtree0

  let devtree1 = analyseWords env devtree0
  putStrLn $ prLinesRTree (prDevNode 2) devtree1
  
  let devtree = combineTrees env devtree1
  putStrLn $ prLinesRTree (prDevNode 4) devtree

  let ts0 = devtree2abstrees devtree
  putStrLn $ unlines $ map prAbsTree ts0

  let ts = map (expandMacro env) ts0
  putStrLn $ unlines $ map prAbsTree ts

  return ts

mkEnv pgf absl cncl = initUDEnv {pgfGrammar = pgf, absLabels = absl, cncLabels = cncl}

-- the pipeline
ud2gf :: UDEnv -> UDTree -> [PGF.Expr]
ud2gf env =
    map abstree2expr
  . devtree2abstrees
  . transformDevTree env
  . udtree2devtree


-- developing tree on the way from UD to GF
type DevTree = RTree DevNode
data DevNode = DevNode {
  devStatus :: [UDId],                  -- indices of subtrees used in the best abstree in DevTrees --- redundant
  devWord   :: String,                  -- the original word
  devTrees  :: [(AbsTree,(CId,[UDId]))], -- trees constructed at this node, with types and used words
  devLemma  :: String,                     -- invariant: devStatus = max (by length) usedSubtrees
  devPOS    :: String,
  devFeats  :: [UDData],
  devLabel  :: String,
  devIndex  :: UDId  -- position in the original sentence
 }
  deriving Show

-- n shows how many trees are to be shown
prDevNode n d = unwords [
  prtStatus (devStatus d),
  devWord d,
  prt (devIndex d),
  devPOS d,
  devLabel d,
  "(" ++ unwords (intersperse ";"
    [prAbsTree e ++ " : " ++ showCId c ++ prtStatus us | (e,(c,us)) <- take n (devTrees d)]) ++ ")",
  show (length (devTrees d))
  ]

maxUsedSubtrees :: DevNode -> [UDId]
maxUsedSubtrees dn = case devTrees dn of
  [] -> []
  t:_ -> snd (snd t) --- because trees are sorted by usage

prAbsTree = showExpr [] . abstree2expr

prtStatus udids =  "[" ++ concat (intersperse "," (map prt udids)) ++ "]"

abstree2expr :: AbsTree -> PGF.Expr
abstree2expr tr@(RTree f ts) = mkApp f (map abstree2expr ts)

devtree2abstrees :: DevTree -> [AbsTree]
devtree2abstrees = map fst . devTrees . root

udtree2devtree :: UDTree -> DevTree
udtree2devtree tr@(RTree un uts) =
  RTree (DevNode {
      devStatus = [],
      devWord  = udFORM un,
      devTrees = [],
      devLemma = udLEMMA un,
      devPOS   = udUPOS un,
      devFeats = udFEATS un,
      devLabel = udDEPREL un,
      devIndex = udID un
     }) (map udtree2devtree uts)

-- this is the main function
transformDevTree :: UDEnv -> DevTree -> DevTree
transformDevTree env =
    addBackups
  . combineTrees env
  . analyseWords env

-- order collected abstract trees by completeness; applied internally in combineTree at each node
rankDevTree :: DevTree -> DevTree
rankDevTree tr@(RTree dn dts) = RTree dn{devTrees = rankSort (devTrees dn)} dts
 where
  rankSort = sortOn ((100-) . rank) -- descending order of rank
  rank (t,(c,us)) = length us

-- omit (t2,(cat,usage2)) if there is (t1,(cat,usage1)) such that usage2 is a subset of usage1
pruneDevTree :: DevTree -> DevTree
pruneDevTree  tr@(RTree dn dts) = RTree dn{devTrees = pruneCatGroups (groupCat (devTrees dn))} dts
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
  
addBackups :: DevTree -> DevTree
addBackups = id ----

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
    tr' | devTrees (root tr') /= devTrees (root tr) -> keepTrying tr'
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
                       [ArgInfo i us (c, devLabel r) e | (e,(c,us)) <- devTrees r]
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
      devTrees = let
                   acu = (funTree finfo,(fst (funTyp finfo),funUsage finfo))
                   dts = devTrees dn
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
      (f,labels) <- M.assocs (funLabels (absLabels env)),
      Just typ   <- [functionType (pgfGrammar env) f]
    ]


analyseWords :: UDEnv -> DevTree -> DevTree
analyseWords env = mapRTree lemma2fun
 where
  lemma2fun dn = dn {
    devTrees = [(t,(c,[devIndex dn])) | (t,c) <- getWordTrees (devLemma dn) (cats (devPOS dn))],
    devStatus = [devIndex dn]
    }
    
  cats pos = maybe [] id $ M.lookup pos (catsForPOS env)

  -- find all functions that are possible parses of the word in any appropriate category
  --- it is still possible that some other category is meant
  getWordTrees w cs = case concatMap (parseWord w) cs of
    [] -> [(newWordTree w c, c) | c <- cs]  -- if no results, just build tree w__c
    fs -> fs  

  --- this can fail if c is discontinuous, or return false positives if w is a form of another word
  parseWord w c = case parse (pgfGrammar env) (actLanguage env) (mkType [] c []) w of
    ts -> [(expr2abstree t,c) | t <- ts]

  newWordTree w c = RTree (mkCId (w ++ "_x_" ++ showCId c)) []
  ---newWordTree w c = RTree (mkCId ("MkNew"++showCId c)) [RTree (mkCId (quote w)) []]

  quote s = "\"" ++ s ++ "\""
