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
import Text.PrettyPrint (render, cat)

import Debug.Trace (trace, traceM, traceShowId)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Monad (unless, forM, when, forM_)
import qualified Data.Set as Set

---------
-- to debug

tracePrint p m x = trace (m ++ " : " ++ p x) x
----traceShow = tracePrint show
traceNoPrint _ _ x = x

-- to test

-- env <- getEnv


getExprs :: [String] -> UDEnv -> String -> [[Expr]]
getExprs rawOpts env string = map getExpr sentences
  where
    eng = actLanguage env
    opts = selectOpts rawOpts
    sentences = map prss $ stanzas $ lines string -- the input string has many sentences

    -- This fun is just showUD2GF without the printing.
    getExpr :: UDSentence -> [Expr]
    getExpr sentence = ts
      where
        udtree = udSentence2tree sentence
        devtree0 = udtree2devtree udtree
        devtree1 = analyseWords env devtree0
        devtree = combineTrees env devtree1
        besttree0 = head (splitDevTree env devtree)
        besttree = addBackups opts besttree0
        ts0 = devtree2abstrees besttree
        ts1 = map (expandMacro env) ts0
        crs = map (checkAbsTreeResult env) ts1
        ts = mapMaybe resultTree crs
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

  ifOptArg opts "dbg" (debugAuxfun env devtree)

  let besttree0 = head (splitDevTree env devtree)
  ifOpt opts "bt0" $ prLinesRTree (prDevNode 1) besttree0

  let besttree = addBackups opts besttree0
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
  resultUnknowns = [f | f <- allNodesRTree t, Nothing <- [asStringLiteral f], Nothing <- [functionType pgf f]],
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

data AbsTreeInfo = AbsTreeInfo
  { atiAbsTree :: AbsTree
  , atiCat :: Cat
  , atiUDIds :: [UDId]
  }
  deriving (Show, Eq)

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
    [prAbsTree e ++ " : " ++ showCId c ++ prtStatus us | AbsTreeInfo e c us <- take n (devAbsTrees d)]) ++ ")",
  show (length (devAbsTrees d))
  ]

devtree2abstrees :: DevTree -> [AbsTree]
devtree2abstrees = map atiAbsTree . devAbsTrees . root

-- to be applied to a DevTree with just one tree at each node
addBackups :: Opts -> DevTree -> DevTree
addBackups opts | isOpt opts "no-backups" = id
                | otherwise = addBackups0 ---- TODO: this must be improved

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
  replaceInfo btrs ai@(AbsTreeInfo ast cat usage ) =
    AbsTreeInfo (replace btrs ast)  cat (sort (nub (concat (usage:map (atiUDIds . snd) btrs))))

  -- check if thre are backups; if not, apply backups to subtrees
  replace :: [(AbsTree,AbsTreeInfo)] -> AbsTree -> AbsTree
  replace btrs tr@(RTree f trs) = case lookup tr btrs of
    Just AbsTreeInfo { atiAbsTree = btr, atiCat = c} -> appBackup c btr (RTree f (map (replace btrs) trs))
    _ -> RTree f (map (replace btrs) trs)

  collectBackup :: DevTree -> (DevTree,(AbsTree,Maybe AbsTreeInfo))
  collectBackup t@(RTree d ts) =
    let ai@AbsTreeInfo { atiAbsTree = ast} = theAbsTreeInfo t in
    (t,(ast, mkBackupList ai [theAbsTreeInfo (addBackups0 u) | u <- ts, devNeedBackup (root u)]))

  mkBackupList :: AbsTreeInfo -> [AbsTreeInfo] -> Maybe AbsTreeInfo
  mkBackupList ai@AbsTreeInfo { atiAbsTree = ast, atiCat = cat, atiUDIds = usage} ts =
    case unzip [(mkBackup a c,us) | AbsTreeInfo { atiAbsTree = a, atiCat = c, atiUDIds = us} <- ts] of
      ([],_) -> Nothing
      (bs,uss) -> Just AbsTreeInfo { atiAbsTree = foldr cons nil bs, atiCat = cat, atiUDIds = sort $ nub $ concat uss}

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
splitDevTree :: UDEnv -> DevTree -> [DevTree]
splitDevTree env tr@(RTree dn trs) =
  [RTree (dn{devAbsTrees = [t]}) (map (chase t) trs) | t <- sortOn isStartCat $ devAbsTrees dn]
 where
  chase AbsTreeInfo { atiAbsTree = ast, atiCat = cat, atiUDIds = usage} tr@(RTree d ts) =
   case elem (devIndex d) usage of
    True -> case sortOn ((1000-) . sizeRTree . atiAbsTree) [dt | dt@AbsTreeInfo { atiAbsTree = t} <- devAbsTrees d, isSubRTree t ast] of
      t:_ -> RTree (d{devAbsTrees = [t]}) (map (chase t) ts)
      _ -> error $ "wrong indexing in\n" ++ prLinesRTree (prDevNode 1) tr
    False -> head $ splitDevTree env $ RTree (d{devNeedBackup = True}) ts ---- head

  isStartCat :: AbsTreeInfo -> Bool
  isStartCat AbsTreeInfo { atiAbsTree = rt, atiCat = ci, atiUDIds = uis} = startCategory env /= mkType [] ci []

prtStatus udids =  "[" ++ concat (intersperse "," (map prt udids)) ++ "]"


-- order collected abstract trees by completeness; applied internally in combineTree at each node
rankDevTree :: DevTree -> DevTree
rankDevTree tr@(RTree dn dts) = RTree dn{devAbsTrees = rankSort (devAbsTrees dn)} dts
 where
  rankSort = sortOn ((100-) . rank) -- descending order of rank
  rank AbsTreeInfo { atiUDIds = us } = length us

-- omit (t2,(cat,usage2)) if there is (t1,(cat,usage1)) such that usage2 is a subset of usage1
pruneDevTree :: DevTree -> DevTree
pruneDevTree  tr@(RTree dn dts) = RTree dn{devAbsTrees = pruneCatGroups (groupCat (devAbsTrees dn))} dts
 where
  cat = atiCat
  usage = atiUDIds
  rank = length . usage
  groupCat = map (sortOn ((100-) . rank)) . groupBy (\x y -> cat x == cat y) . sortOn cat
  prune usages grp = case grp of
    t:ts | any (\u -> all (\x -> elem x u) (usage t)) usages -> prune usages ts
    t:ts -> t : prune (usage t : usages) ts
    _ -> grp
  pruneCatGroups = concatMap (prune [])

debugAuxfun :: UDEnv -> DevTree -> String -> String
debugAuxfun env dt funArg
  | (funName:args) <- words funArg
  , funCid <- mkCId funName
  , Just argsI <- traverse (getInt . reads) args = debugAuxFun' env dt funCid argsI
  | otherwise = error "Usage: dbg='FunName 4 9 2' where the numbers represent word numbers"

-- Check for successful int parse
getInt :: [(Int, String)] -> Maybe Int
getInt [(n,"")] = Just n
getInt _ = Nothing

debugAuxFun' :: UDEnv -> DevTree -> CId -> [Int] -> String
debugAuxFun' env dt funId argNrs = either ("Error: " ++) id $ do
  traceM $ "\nStarting debug for " ++ showCId funId ++ ":"
  unless (M.notMember funId (disabledFunctions (cncLabels env))) $
    Left $ "The function " ++ showCId funId ++ " is disabled"

  let showAttrs [] = ""
      showAttrs xs = "[" ++ intercalate "," (map prt xs) ++ "]"
  let showFun outCat argCatLabs = show funId ++ " : " ++ intercalate " -> " (map (show . fst) argCatLabs) ++ " -> " ++ show outCat ++ " ; "
       ++ unwords ([ lab ++ showAttrs b | (_,(lab,b)) <- argCatLabs])

  -- Find the function definition
  (f,(outCat, argCatLabs)) <- case [(f,labtyp) | (f,labtyp) <- allFunsEnv env, f == funId] of
    [] -> Left $ "Unknown function: " ++ show funId
    [(f,labtyp)] -> pure (f,labtyp)
    fs -> Left $ "Mulitple labels found for function " ++ show funId ++ "\n" ++
                unlines (map (uncurry showFun . snd) fs)
  let showTheFun = showFun outCat argCatLabs
  traceM showTheFun
  unless (length argCatLabs == length argNrs) $ Left $ "Wrong number of arguments: " ++ show argNrs ++ " (expected " ++ show (length argCatLabs) ++ " args) for "
         ++ showTheFun

  let catLabNrs = zip argNrs argCatLabs
  let catlabHeads = filter (\(nr,(cat,(lab,feats))) -> lab == head_Label) catLabNrs
  (headNr, catlabHead) <- case catlabHeads of [ch] -> pure ch; _ -> Left ("Missing head label for function: " ++ show f ++ "\nlabels: " ++ unwords (map (fst . snd) argCatLabs))

  -- Step 1. find where the head is in the tree
  headTree <- case findNode env (UDIdInt headNr) dt of
    [] -> Left $ "Head node not found: " ++ show headNr
    [rt] -> pure rt
    (_:_:_) -> Left $ "Multiple head nodes: " ++ show headNr
  let headNode = root headTree

  let showWord nr = case findNode env (UDIdInt nr) dt of [rt] -> show (devWord (root rt)); _ -> "<not found>"
  let showIdSimple (UDIdInt nr) = show nr
      showIdSimple nr = show nr

  -- Step 2. Verify that all arguments are children of the head
  argNodes <- forM catLabNrs $ \(nr,catlab) -> case find ((== UDIdInt nr) . devIndex) ((headNode{devLabel=head_Label}): map root (subtrees headTree)) of
    Nothing -> Left $ "Word number " ++ show nr ++ " (" ++ showWord nr ++ ") " ++ " is not a child of " ++ show headNr ++ " (" ++ show (devWord headNode) ++ ").\n"
            ++ "    Available children: " ++ show [(showIdSimple (devIndex t), devWord t) | t <- map root $ subtrees headTree]
    Just rt -> pure (rt,catlab)
  traceM $ "Attempting to build: " ++ showCId funId ++ " " ++ unwords [ devWord nd | (nd,_) <- argNodes]
  -- let allArgNodes = [(nd,catlab) | nr <- argNrs , (nd,catlab) <- (headNode,catlabHead): argNodes, devIndex nd == UDIdInt nr]

  -- -- 3. Check if any version of the head is compatible with the function
  -- let missingHeadFeats = filter (`notElem` devFeats headNode) $ snd (snd catlabHead)
  -- unless (null missingHeadFeats) $ Left $ "Missing head features: " ++ showAttrs missingHeadFeats ++ " for " ++ showCId f ++ " with head \"" ++ devWord headNode
  --   ++ "\". Head features: " ++ showAttrs (devFeats headNode)
  -- let headAT = devAbsTrees headNode
  -- let goodTrees = [ x | x <- headAT , atiCat x == fst catlabHead]
  -- when (null goodTrees) $ Left $ "No trees with expected category for " ++ showCId f ++ " with head \"" ++ devWord headNode ++ "\"\n"
  --   ++ "Expected category: " ++ show (fst catlabHead) ++ "\n"
  --   ++ "Available categories: " ++ show (map atiCat headAT)
  -- traceM $ "Found head trees with correct category: " ++ intercalate "\n" (map (prRTree showCId . atiAbsTree) goodTrees)

  -- 4. Check that the arguments are compatible with the function
  let badLabels = [(node, lab) | (node, (cat,(lab,feats))) <- argNodes, devLabel node /= lab]
  unless (null badLabels) $ Left $ ("Incompatible argument labels:\n" ++) $
    intercalate "\n" [ " - For " ++ show (devWord node) ++ ": Got " ++ devLabel node ++ " expected " ++ lab | (node,lab) <- badLabels]
  let badAttrs = [(node, missingFeats) | (node, (cat,(lab,feats))) <- argNodes, let missingFeats = filter (`notElem`devFeats node) feats, not (null missingFeats)]
  unless (null badAttrs) $ Left $ ("Missing argument features:\n" ++) $
    intercalate "\n" [ " - For " ++ show (devWord node) ++ ": Missing features " ++ showAttrs feats ++ " from " ++ showAttrs (devFeats node) | (node,feats) <- badAttrs]

  -- Check the category of the arguments
  -- TODO: Be less confusing when the node is deeply nested because of pruning
  forM_ argNodes $ \(node,(cat,(lab,feats))) -> do
    traceM $ "\nArgument " ++ show (devWord node) ++ " : " ++ showCId cat ++ " ; " ++ lab ++ showAttrs feats ++ ":"
    let nodeAT = devAbsTrees node
    let goodTrees = [ x | x <- nodeAT , atiCat x == cat]
    when (null goodTrees) $ Left $ "No trees with expected category for " ++ showCId f ++ " with arg \"" ++ devWord node ++ "\"\n"
      ++ "Expected category: " ++ show cat ++ "\n"
      ++ "Available categories: " ++ show (map atiCat nodeAT)
    traceM $ "  Found trees with correct category:\n    - " ++ intercalate "\n    - " (map ((++ " : " ++ show cat) . prRTree showCId . atiAbsTree) goodTrees)

  -- 5: Check that the constructed tree exists in the dev-tree
  let headAT = devAbsTrees headNode
  let matchingAbstrees = [ x | x <- headAT , root (atiAbsTree x) == f, all ((`elem` atiUDIds x) . UDIdInt) argNrs]
  let sameCategory = [ x | x <- headAT, atiCat x == outCat]
  when (null matchingAbstrees) $ Left $ "Can make tree, but tree not found in devtree. Found trees with same result category as "++ showCId f ++ ": " ++ intercalate "\n    " (map ((++ " : " ++ show outCat) . prRTree showCId . atiAbsTree) sameCategory)
  traceM $ "Trees using " ++ showCId f ++ " found in devtree:\n    " ++ intercalate "\n    " (map ((++ " : " ++ show outCat) . prRTree showCId . atiAbsTree) matchingAbstrees)

  -- TODO: Check if the tree would be selected

  --pure $ unlines [showFun, show funId, show argNrs, show argCatLabs, show headNr, prLinesRTree (prDevNode 3) headTree]
  pure "Success!"

findNode :: UDEnv -> UDId -> DevTree -> [DevTree]
findNode env nr dt@(RTree dn rts)
  | devIndex dn == nr = pure dt
  | otherwise = findNode env nr =<< rts
  -- Steps:




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
  deriving Show

combineTrees :: UDEnv -> DevTree -> DevTree
combineTrees env =
     rankDevTree
   . comb

 where

  comb :: DevTree -> DevTree
  comb tr@(RTree dn dts) = case map comb dts of
    ts -> traceNoPrint (prDevNode 3 . root) "built" $ pruneDevTree $ rankDevTree $ keepTryingNew (RTree dn ts)

  -- Apply all possible functions and iterate doing the same on the new trees until there's no new trees
  keepTryingNew :: DevTree -> DevTree
  keepTryingNew tr = tryEvenMore fs tr
    where fs = newFuns (allFunsLocalFast tr) tr

  -- Apply all possible functions to the GF trees that were created in the previous iteration
  tryEvenMore :: [FunInfo] -> DevTree -> DevTree
  tryEvenMore [] tr = tr
  tryEvenMore fis tr@RTree{root=nd} = tryEvenMore (newFuns (allFunsLocalFast onlyNewTree) nextTr) nextTr
    where
      -- The head only contains new trees that were created in the previous iteration
      onlyNewTree = tr {root = nd { devAbsTrees = map funInfoToAbsTreeInfo fis}}
      -- Add the new trees to the old ones. There shouldn't really be any duplicates now, so there's
      -- a bit of redundant checking going on here.
      nextTr = combineUnduplicated fis tr

  combineUnduplicated :: [FunInfo] -> DevTree -> DevTree
  combineUnduplicated finfos tree@(RTree dn ts)=
    RTree dn{
      devAbsTrees = oldDevTrees ++ newDevTrees,
                               -- Newer suggestions are added to the end of the list, which prefers flatter trees.
                               -- Consider a tree like         A
                               --                                B
                               --                                C
                               -- which we can make into a GF tree 2 different ways:
                               -- (i) ShallowFun A B C , or (ii) DeepFun (SubFun A B) C.
                               -- Round 1: construct ShallowFun A B C and SubFun A B.
                               -- Round 2: construct DeepFun, because now we have (SubFun A B).
                               -- The list of devtrees undergoes many reorderings throughout the program, but
                               -- this choice, oldDevTrees++newDevTrees or newDevTrees++oldDevTrees determines the order of (i) and (ii).
      devStatus = maximumBy (comparing length) (devStatus dn : map funUsage finfos)
      } ts
    where
      --  traceWith x = trace (show $ fmap (prAbsTree . atiAbsTree) x) x 
        traceWith x = x
        newDevTrees = traceWith $ funInfoToAbsTreeInfo <$> finfos
        oldDevTrees = devAbsTrees dn

  allFunsLocalFast :: DevTree -> [FunInfo]
  allFunsLocalFast (RTree dn ts)=
    [FunInfo f labtyp abstree usage |
    -- for head and each immediate subtree, build the list of its already built abstrees, each with type and label
    -- argalts :: [[Arg]] -- one list for root and for each subtree
    let argalts =
         [
           (devIndex r, [ArgInfo i us (c, devLabel r) (devFeats r) e | AbsTreeInfo { atiAbsTree = e, atiCat = c, atiUDIds = us} <- devAbsTrees r])
         |
         -- number the arguments: root node 0, subtrees 1,2,..
         (i,r) <- (0,dn{devLabel = head_Label}) : zip [1..] (map root ts)
         ],

    (f,labtyp) <- allFunsEnv env,
    (abstree,usage) <- tryFindArgsFast f labtyp argalts,
    not $ isLooping abstree
    ]

  -- NOTE: argss is transposed compared to tryFindArgs
  tryFindArgsFast :: CId -> LabelledType -> [(UDId, [ArgInfo])] -> [(AbsTree,[UDId])]
  tryFindArgsFast  f (_, catlabs) (headArgs:argss) =
    [ (abstree,usage)
    | let catlabHeads = filter (\(cat,(lab,feats)) -> lab == head_Label) catlabs
    , let catlabHead = case catlabHeads of [ch] -> ch; _ -> error ("Missing head label for function: " ++ show f ++ "\nlabels: " ++ unwords (map (fst . snd) catlabs))
      -- Select a headArg matching labcatHead
      -- Filter out argss according to use from the headArg
      -- Select other args until done
    , headArg <- snd headArgs
    , singleArgTypeMatches catlabHead headArg
    , let headUsage = argUsage headArg -- TODO Use Set or IntSet for argUsage
    , let unusedArgs = filter ((`notElem` headUsage) . fst) argss -- Don't include arguments used by the head
    , dependentArgs <- findOtherArgs headArg headUsage catlabs unusedArgs
    , let allArgs = dependentArgs
    , let abstree = RTree f (map argTree allArgs)
    , let usage = sort (concatMap argUsage allArgs) -- head usage + dependents' argument numbers

    ]
  tryFindArgsFast  f (_, catlabs) [] = error "Avoidable partiality" -- TODO: Replace with e.g NonEmpty

  -- Find non-head arguments for a function
  findOtherArgs :: ArgInfo -> [UDId] -> [(Cat,(Label,[UDData]))] -> [(UDId, [ArgInfo])] -> [[ArgInfo]]
  findOtherArgs _ usage [] argss = [[]]
  findOtherArgs headArg usage (catlab : catlabs) argss
    | fst (snd catlab) == head_Label = map (headArg :) $ findOtherArgs headArg usage catlabs argss
  findOtherArgs headArg usage (catlab : catlabs) argss =
    [ arg : remaining
    | ((argNr, args), unusedArgs) <- select argss -- Pick any subtree
    , arg <- args                                 -- Select any alternative from it
    , all (`notElem` usage) $ argUsage arg        -- That doesn't overlap with already used args -- TODO Probably not needed, since
    , singleArgTypeMatches catlab arg             -- And matches the signature of the function   --      subtrees shouldn't overlap
    , remaining <- findOtherArgs headArg (argUsage arg ++ usage) catlabs unusedArgs
    ]

  singleArgTypeMatches :: (Cat,(Label,[UDData])) -> ArgInfo -> Bool
  singleArgTypeMatches catlab@(cat,(lab,feats)) arg =
    argCatLab arg == (cat,lab) &&
    all (`elem` argFeats arg) feats -- required features are found ---- TODO if feats or (argFeats arg) contain disjunctions

  funInfoToAbsTreeInfo :: FunInfo -> AbsTreeInfo
  funInfoToAbsTreeInfo finfo = AbsTreeInfo { atiAbsTree = funTree finfo, atiCat = fst $ funTyp finfo, atiUDIds = funUsage finfo}

  newFuns :: [FunInfo] -> DevTree -> [FunInfo]
  newFuns fis (RTree dn rts) = result
    where
      dts = devAbsTrees dn
      result = filter ((`notElem` dts) . funInfoToAbsTreeInfo) fis

isLooping :: AbsTree -> Bool
isLooping = go Set.empty
  where
    go :: Set.Set Fun -> AbsTree -> Bool
    go seen tr@(RTree fn [nxt]) 
      | fn `Set.member` seen = trace ("Looping set: " ++ show (Set.toList seen) ++ " on " ++ take 100 (prAbsTree tr)) True
      | otherwise = go (Set.insert fn seen) nxt
    go seen (RTree fn _) = False
    notrace x = id

analyseWords :: UDEnv -> DevTree -> DevTree
analyseWords env = mapRTree lemma2fun
 where
  morpho = buildMorpho (pgfGrammar env) (actLanguage env)
  lemma2fun dn = dn {
    devAbsTrees = [AbsTreeInfo { atiAbsTree = t, atiCat = c, atiUDIds = [devIndex dn]} | (t,c) <- justWords],
    devStatus = [devIndex dn],
    devIsUnknown = isUnknown
    }
   where
    (isUnknown,justWords) = getWordTrees (devWord dn) (devLemma dn) (cats (devPOS dn))

  cats pos = maybe [] (map (either (Left. fst) Right)) $ M.lookup pos (catsForPOS env)

  -- find all functions that are possible parses of the word in any appropriate category
  --- it is still possible that some other category is meant
  getWordTrees wf w cs = case concatMap (parseWord w) cs `ifEmpty` concatMap (parseWord (map toLower w)) cs `ifEmpty` morphoFallback wf of
    [] -> case cs of
      [] -> (True,[(newWordTree wfLiteral unknownCat, unknownCat)])
      _  -> (True,[(newWordTree wfLiteral ec, ec) | c <- cs, let ec = either id id c, strFunExists ec]
                   `ifEmpty` [(newWordTree w ec, ec) | c <- cs, let ec = either id id c])
    fs -> (False,fs)
    where
      isAllCaps = all isUpper
      isSame str1 str2 = map toLower str1 == map toLower str2
      wfLiteral = if isAllCaps wf && isSame w wf then wf else w


  -- | Return the first non-empty list
  ifEmpty [] xs = xs
  ifEmpty xs _  = xs
  infixr 3 `ifEmpty`

  -- Verify that a StrSomeCat function exists in grammar
  strFunExists c | Just typ  <- functionType (pgfGrammar env) f = True
                 | otherwise                                    = False
      where f = mkCId ("Str" ++ showCId c)

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

  -- Fall back to morphoanalysis if gf parse fails
  -- TODO: We might want to use the morphoanalysis for all words, not just when parse fails
  morphoFallback :: String -> [(RTree CId, CId)]
  morphoFallback w =
    [(RTree name [], c)
    | (name, _) <- lookupMorpho morpho w
    , Just tp <- pure $ functionType (pgfGrammar env) name
    , ([], c, []) <- pure $ unType tp
    ]

-- auxiliaries
-- newWordTree w c = RTree (mkCId (w ++ "_" ++ showCId c)) [] ---
newWordTree w c = RTree (mkCId ("Str" ++ showCId c)) [strLitToAbsTree w] ---
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
      devFeats = udFEATS un ++ [UDData "LEMMA" [udLEMMA un]
                               ,UDData "FORM"  [udFORM un]
                               ,UDData "XPOS"  [udXPOS un]
                               ,UDData "POS"   [udUPOS un]]
                               ++ map (\udData -> udData{udArg = "MISC-" ++ udArg udData}) (udMISC un),
      devLabel = udDEPREL un,
      devIndex = udID un,
      devClosest = udIdRoot, --- junk value
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

--  | Remove any element from a list
-- >>> select [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
select :: [a] -> [(a,[a])]
select [] = []
select (a : as) = (a,as) : [ (b,a:bs) | (b,bs) <-select as ]