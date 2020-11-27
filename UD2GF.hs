module UD2GF where

import RTree
import UDConcepts
import UDAnnotations
import GFConcepts
import UDOptions
import UDVisualization
import Backend

import PGF hiding (CncLabels)

import qualified Data.Map as M
import Data.List
import Data.Char
import Data.Maybe
import Text.PrettyPrint (render)

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
  if isOpt opts "vat" then (visualizeAbsTrees env (map expr2abstree (concatMap fst tstats))) else return ()
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

  let ts1 = map (expandMacro env) ts0
  ifOpt opts "at" $ unlines $ map prAbsTree ts1

  let crs = map (checkAbsTreeResult env) ts1
  ifOpt opts "tc" $ unlines $ map prCheckResult crs
  let ts = [t | Just t <- map resultTree crs]

  if null ts then return () else
    ifOpt opts "lin" (unlines $ map (("LIN: " ++) . linearizeTree env (actLanguage env)) ts)

  if isOpt opts "sum"
    then do
        putStrLn "#sum, an extractive summary (tree built from interpreted nodes)"
        let sts0 = devtree2abstrees besttree0
        let sts1 = map (expandMacro env) sts0
        ifOpt opts "at" $ unlines $ map prAbsTree sts1 
        let scrs = map (checkAbsTreeResult env) sts1
        ifOpt opts "tc" $ unlines $ map prCheckResult scrs
        let sts = [t | Just t <- map resultTree scrs]
        if null sts then return () else
          ifOpt opts "lin" (unlines $ map (("SUMMARY LIN: " ++) . linearizeTree env (actLanguage env)) sts)
    else return ()
  
  let allnodes = allNodesRTree besttree0
      orig = length allnodes
      interp = length (devStatus (root besttree0))
      stat = UD2GFStat {
       totalWords = orig,
       interpretedWords = interp,
       unknownWords = length [dn | dn <- allnodes, devIsUnknown dn],
       totalSentences = 1,
       completeSentences = div interp orig, -- either 1 or 0
       typecorrectSentences = min 1 (length ts)  -- 1 if type-correct, 0 if not
       }
  
  return (ts,stat)


data UD2GFStat = UD2GFStat {
  totalWords :: Int,
  interpretedWords :: Int,
  unknownWords :: Int,
  totalSentences :: Int,
  completeSentences :: Int,
  typecorrectSentences :: Int
  }
 deriving Show

prUD2GFStat :: UD2GFStat -> String
prUD2GFStat stat = unlines $ [
  "total word nodes:\t"                    ++ show (totalWords stat),
  "interpreted word nodes:\t"              ++ show (interpretedWords stat) ++ proportion interpretedWords totalWords,
  "unknown word nodes (tokens):\t"         ++ show (unknownWords stat) ++ proportion unknownWords totalWords,
  "total sentences:\t"                     ++ show (totalSentences stat),
  "completely interpreted sentences:\t"    ++ show (completeSentences stat) ++ proportion completeSentences totalSentences,
  "type-correct sentences:\t"              ++ show (typecorrectSentences stat) ++ proportion typecorrectSentences totalSentences
  ]
 where
   proportion f g = " (" ++ show (div (100 * f stat) (g stat)) ++ "%)"

combineUD2GFStats :: [UD2GFStat] -> UD2GFStat
combineUD2GFStats stats = UD2GFStat {
  totalWords = sum (map totalWords stats),
  interpretedWords = sum (map interpretedWords stats),
  unknownWords = sum (map unknownWords stats),
  totalSentences = sum (map totalSentences stats),
  completeSentences = sum (map completeSentences stats),
  typecorrectSentences = sum (map typecorrectSentences stats)
  }

data CheckResult = CheckResult {
  resultTree     :: Maybe Expr,
  resultUnknowns :: [Fun],
  resultMessage  :: String
  }
 deriving Show

prCheckResult cr = unlines $ 
  case resultUnknowns cr of
    [] -> []
    uks -> [unwords $ "unknown words:" : map showCId uks]
  ++
  [resultMessage cr]

-- check the resulting tree
checkAbsTreeResult :: UDEnv -> AbsTree -> CheckResult
checkAbsTreeResult env t = CheckResult {
  resultTree = mt,
  resultUnknowns = [f | f <- allNodesRTree t, Nothing <- [functionType pgf f]],
  resultMessage = msg
  }
 where
  pgf = pgfGrammar env
  (mt,msg) = case inferExpr pgf (abstree2expr t) of
    Left tce -> (Nothing, render (ppTcError tce))
    Right (exp,typ) -> (Just exp, "type checking OK")


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
  devClosest    :: UDId,   -- closest word, either next or previous depending on dominance
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
  "("++prt (devClosest d)++")",
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
addBackups = addBackups0 ---- TODO: this must be improved

addBackups0 :: DevTree -> DevTree
addBackups0 tr@(RTree dn trs) = case map collectBackup (tr:trs) of  -- backups from the tree itself and every subtree
  btrs -> RTree
    (dn {devAbsTrees = [
           replaceInfo [(t,ai) | (_,(t,Just ai)) <- btrs]   -- 
           (theAbsTreeInfo tr)]                             -- the only abstree that there is 
        }
    )
    (map fst (tail btrs))
  
 where

  -- add backups to tree, update usage with the nodes used in the backups (if no backups, do nothing)
  replaceInfo :: [(AbsTree,AbsTreeInfo)] -> AbsTreeInfo -> AbsTreeInfo
  replaceInfo btrs ai@(ast,(cat,usage)) =
    (replace btrs ast,(cat,sort (nub (concat (usage:map (snd . snd . snd) btrs)))))

  -- check if thre are backups; if not, apply backups to subtrees
  replace :: [(AbsTree,AbsTreeInfo)] -> AbsTree -> AbsTree
  replace btrs tr@(RTree f trs) = case lookup tr btrs of
    Just (btr,(c,_)) -> appBackup c btr (RTree f (map (replace btrs) trs))
    _ -> RTree f (map (replace btrs) trs)

  collectBackup :: DevTree -> (DevTree,(AbsTree,Maybe AbsTreeInfo))
  collectBackup t@(RTree d ts) =
    let ai@(ast,_) = theAbsTreeInfo t in
    (t,(ast, mkBackupList ai [theAbsTreeInfo (addBackups0 u) | u <- ts, devNeedBackup (root u)]))

  mkBackupList :: AbsTreeInfo -> [AbsTreeInfo] -> Maybe AbsTreeInfo
  mkBackupList ai@(ast,(cat,usage)) ts =
    case unzip [(mkBackup a c,us) | (a,(c,us)) <- ts] of
      ([],_) -> Nothing
      (bs,uss) -> Just (foldr cons nil bs, (cat,sort (nub (concat uss))))

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

-- function application to a given set of arguments when building up DevTree
data FunInfo = FunInfo {
  funFun   :: Fun,            -- GF function
  funTyp   :: LabelledType,   -- its type with matching labels
  funTree  :: AbsTree,        -- tree that would be built with the available arguments
  funUsage :: [UDId]          -- subtrees that are consumed as arguments
  }

data ArgInfo = ArgInfo {
  argNumber :: Int,           -- how manieth subtree
  argUsage  :: [UDId],        -- what subtrees it consumes
  argCatLab :: (Cat,Label),   -- its type and the label of its head word
  argFeats  :: [UDData],      -- features of its head word
  argTree   :: AbsTree        -- the GF tree built at that node
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
        (f,labtyp) <- allFunsEnv env,

        -- for head and each immediate subtree, build the list of its already built abstrees, each with type and label
        -- argalts :: [[Arg]] -- one list for root and for each subtree
        let argalts = [
                       [ArgInfo i us (c, devLabel r) (devFeats r) e | (e,(c,us)) <- devAbsTrees r]
                           |
                             (i,r) <- (0,dn{devLabel = head_Label}) :  -- number the arguments: root node 0, subtrees 1,2,..
                                                 [(i,r) | (i,r) <- zip [1..] (map root ts)]
                      ],

        -- argument sequences: an argument whose index is already in [Int] may not be used
        -- argseqsAfter :: [Int] -> [[Arg]] -> [[Arg]]
        let argseqsAfter us argss =
--- too slow!              [filter (\x -> all (flip notElem us) (argUsage x)) xs  | xs <- sequence argss],
              sequence (filter (not . null) [filter (\x -> all (flip notElem us) (argUsage x)) xs  | xs <- argss]),

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

  argTypeMatches :: [(Cat,(Label,[UDData]))] -> [ArgInfo] -> [[(AbsTree,[UDId])]]
  argTypeMatches catlabs args = case catlabs of
    catlab1@(cat,(lab,feats)):catlabs2 -> [
      [(t,i) | (t,i) <- (argTree x, argUsage x):xs]
        |
          x  <- [arg | arg <- args,
                       argCatLab arg == (cat,lab),
                       all (\f -> elem f (argFeats arg)) feats -- required features are found ---- TODO if feats or (argFeats arg) contain disjunctions
                ],
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
                    ---- || length dts > 123 ---- TODO parameterize "beam" size
                   then dts
                   else acu:dts,
      devStatus = maximumBy (\x y -> compare (length x) (length y)) [devStatus dn, funUsage finfo]
      } ts

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
      [] -> (True,[(newWordTree w unknownCat, unknownCat)])
      _  -> (True,[(newWordTree w ec, ec) | c <- cs, let ec = either id id c])

    fs -> (False,fs)

  --- this can fail if c is discontinuous, or return false positives if w is a form of another word
  parseWord w ec = case ec of
    Left c -> case parse (pgfGrammar env) (actLanguage env) (mkType [] c []) w of
      ts -> [(at,c) | t <- ts,
                      let at = expr2abstree t,
                      all (\f -> M.notMember f (disabledFunctions (cncLabels env))) (allNodesRTree at)]
    Right c -> case elem (w,c) auxWords of
      True -> [(newWordTree w c, c)]
      _ -> []

  auxWords = [(lemma,cat) | ((fun_,lemma),(cat,labels_)) <- M.assocs (lemmaLabels (cncLabels env))]

-- auxiliaries 
newWordTree w c = RTree (mkCId (w ++ "__x__" ++ showCId c)) [] ---
isNewWordFun f = isInfixOf "__x__" (showCId f)
unknownCat = mkCId "Adv" --- treat unknown words as adverbs ---- TODO: from config
quote s = "\"" ++ s ++ "\""

-- initialize the process
udtree2devtree :: UDTree -> DevTree
udtree2devtree = markClosest . initialize

 where
 
  initialize tr@(RTree un uts) =
    RTree (DevNode {
      devStatus = [],
      devWord  = udFORM un,
      devAbsTrees = [],
      devLemma = udLEMMA un,
      devPOS   = udUPOS un,
      devFeats = udFEATS un,
      devLabel = udDEPREL un,
      devIndex = udID un,
      devClosest = UDIdRoot, --- junk value
      devNeedBackup = False, ---- TODO start with True, mark when used
      devIsUnknown = True
     }) (map initialize uts)

  markClosest tr@(RTree dn dts) = 
    RTree (dn {
       devClosest = hardClosest (devIndex dn)  -- top node not dominated
      }) (map (mark (devIndex dn)) dts)

  mark ui tr@(RTree dn dts) =
    let dui = devIndex dn
    in RTree (dn {
         devClosest = if False ---- elem dui [previousUDId ui,nextUDId ui] -- problem: circularity i<->i+1
                        then ui
                        else hardClosest dui
         }) (map (mark dui) dts)

  hardClosest ui =
    if udPosition ui == 1
      then nextUDId ui -- first word linked to next one --- does not work for one-word sentence
      else previousUDId ui  -- other words to previous ones, also works for the last word
      