module UDAnnotations where

import UDConcepts
import PGF hiding (CncLabels)
import RTree
import GFConcepts

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Maybe
import System.FilePath.Posix (takeBaseName)

data UDEnv = UDEnv {
  udFormat    :: String, -- default .conllu
  absLabels   :: AbsLabels,  -- language-independent labels
  cncLabels   :: CncLabels, -- language-dependent labels, even those that work on AST only
  pgfGrammar  :: PGF,
  actLanguage :: Language,
  startCategory :: PGF.Type
  }

initUDEnv =
  UDEnv "conllu" initAbsLabels initCncLabels (error "no pgf") (error "no language") (error "no startcat")

mkUDEnv pgf absl cncl eng cat =
  initUDEnv {pgfGrammar = pgf, absLabels = absl, cncLabels = cncl, actLanguage = eng, startCategory = maybe undefined id $ readType cat}

getEnv :: String -> String -> String -> IO UDEnv
getEnv pref eng cat = do
  pgf <- readPGF (stdGrammarFile pref)
  abslabels <- readFile (stdAbsLabelsFile pref) >>= return . pAbsLabels
  cnclabels <- readFile (stdCncLabelsFile pref eng) >>= return . pCncLabels
  let actlang = stdLanguage pref eng
  let env = mkUDEnv pgf abslabels cnclabels actlang cat
  return $ addMissing env

getAnnotEnv :: [FilePath] -> IO UDEnv
getAnnotEnv files@(file:fs) = do
  abslabels <- readFile file >>= return . pAbsLabels
  case fs of
    gfile:_ -> do
      pgf <- readPGF gfile
      return $ initUDEnv {absLabels = abslabels, pgfGrammar = pgf}
    _ -> return $ initUDEnv {absLabels = abslabels}

checkAnnotations :: String -> String -> String -> IO ()
checkAnnotations pref eng cat = do
  env <- getEnv pref eng cat
  putStrLn $ unlines $ checkAbsLabels env (absLabels env)
  putStrLn $ unlines $ checkCncLabels env (cncLabels env)

stdLanguage pref eng = mkLanguage (takeBaseName pref ++ eng)
stdGrammarFile pref = pref ++ ".pgf"
stdAbsLabelsFile pref = pref ++ ".labels"
stdCncLabelsFile pref eng = pref ++ eng ++ ".labels"
mkLanguage =  maybe undefined id . readLanguage

isEnvUD2 env = annotGuideline (absLabels env) == Just "UD2"

parseEng env s = head $ parse (pgfGrammar env) (actLanguage env) (startCategory env) s

data AbsLabels = AbsLabels {
  annotGuideline    :: Maybe String,
  funLabels         :: M.Map CId [([Maybe CId], [Label])],
  catLabels         :: M.Map CId (String,Bool) -- True marks primary category in ud2gf
  }

initAbsLabels :: AbsLabels
initAbsLabels = AbsLabels (Just "UD2") M.empty M.empty

-- is be VERB cop head
data CncLabels = CncLabels {
  wordLabels     :: M.Map String (String,[UDData]),         -- word -> (lemma,morpho)                    e.g. #word been be Tense=Past|VerbForm=Part
  lemmaLabels    :: M.Map (Fun,String) (Cat,(Label,Label)), -- (fun,lemma) -> (auxcat,(label,tgtLabel)), e.g. #lemma DEFAULT_ be Cop cop head
  morphoLabels   :: M.Map (Cat,Int) [UDData],               -- (cat,int) -> morphotag,                   e.g. #morpho V,V2,VS 0 VerbForm=Inf
  discontLabels  :: M.Map (Cat,Int) (POS,Label,Label),      -- (cat,field) -> (pos,label,target)         e.g. #discont  V2  5,ADP,case,obj   6,ADV,advmod,head
  multiLabels    :: M.Map Cat (Bool, Label),                -- cat -> (if-head-first, other-labels)      e.g. #multiword Prep head first fixed
  auxCategories  :: M.Map CId String,                       -- auxcat -> cat, in both gf2ud and ud2gf,   e.g. #auxcat Cop AUX
  changeLabels   :: M.Map Label [(Label,Condition)],        -- change to another label afterwards        e.g. #change obj>obl above case 
  macroFunctions :: M.Map CId (AbsType,(([CId],AbsTree),[(Label,[UDData])])), -- ud2gf only,         e.g. #auxfun MkVPS_Fut will vp : Will -> VP -> VPS = MkVPS (TTAnt TFut ASimul) PPos vp ; aux head
  altFunLabels   :: M.Map CId [[Label]],                                      -- ud2gf only,         e.g. #altfun ComplSlash head obl
  disabledFunctions :: M.Map Fun ()                                           -- not to be used in ud2gf, e.g. #disable the_Det thePl_Det

  }

data Condition =
    CAbove Label        -- to change a label if it dominates this label
  | CFeatures [UDData]  -- if it has these features
  
initCncLabels = CncLabels M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

-- check the soundness of labels

checkAbsLabels :: UDEnv -> AbsLabels -> [String]
checkAbsLabels env als =
  ---- check completeness, too, as well as if a function is included twice
  concatMap chFun funs ++
  ["no annotation for function " ++ showCId f |  -- not needed if 0 or 1 args
       (f,(_,args)) <- allfuns,
       length args > 1,
       Nothing <- [M.lookup f (funLabels als)]] ++ 
  concatMap chCat cats ++
  take 1 ["no annotation for category " ++ showCId c |  -- needed only if cat has 0-place functions
       c <- categories pgf, 
       Nothing <- [M.lookup c (catLabels als)],
       _ <- [() | (f,(val,[])) <- allfuns, c==val]
       ] 

 where
  pgf  = pgfGrammar env
  funs = M.toList (funLabels als)
  cats = M.toList (catLabels als)
  allfuns = pgf2functions pgf

  chFun (f,ls) =
    ["unknown function " ++ showCId f | notElem f (map fst allfuns)] ++
    ["no head in "       ++ showCId f | (_,ll) <- ls, notElem "head" ll] ++
    ["wrong number of labels in " ++ showCId f ++ " : " ++ showType [] typ |
      Just typ <- [functionType pgf f],
      let (hs,_,_) = unType typ,
      (_,ll) <- ls,
      length hs /= length ll
      ]  ---- TODO check validity of nonlocal patterns
      ++
      if (isEnvUD2 env) then concatMap checkUDLabel (filter (/="head") (concatMap snd ls)) else []

  chCat (c,(p,_)) =
    ["unknown category " ++ showCId c | notElem c (categories pgf)] ++
    if (isEnvUD2 env) then checkUDPOS p else []
---- TODO also check if primary is unique

-- get the labels from file

pAbsLabels :: String -> AbsLabels
pAbsLabels = dispatch . map words . uncomment . lines
 where
  dispatch = foldr add initAbsLabels
  add ws labs = case ws of
    "#guidelines":w:_   -> labs{annotGuideline = Just w} --- overwrites earlier declaration
    "#fun":f:xs | elem ">" xs -> labs{funLabels = insertFunLabels (mkCId f) (map getMaybeFun fs, ls) (funLabels labs)} where (fs,_:ls) = break (==">") xs
    "#fun":f:xs         -> labs{funLabels = insertFunLabels (mkCId f) (unzip (map pFunLabel xs)) (funLabels labs)}
    "#cat":c:p:[]       -> labs{catLabels = M.insert (mkCId c) (p,False) (catLabels labs)}
    "#cat":c:p:"primary":[] -> labs{catLabels = M.insert (mkCId c) (p,True) (catLabels labs)}
    
  --- fun or cat without keywords, for backward compatibility
    f:xs@(_:_:_)        -> labs{funLabels = M.insert (mkCId f) [(replicate (length xs) Nothing, xs)] (funLabels labs)}
    f:"head":[]         -> labs{funLabels = M.insert (mkCId f) [([Nothing],[head_Label])] (funLabels labs)}
    c:p:[]              -> labs{catLabels = M.insert (mkCId c) (p,False) (catLabels labs)}

 --- ignores ill-formed lines silently
    _ -> labs 

  insertFunLabels f (ps,ls) funlabs = case M.lookup f funlabs of
    Just psls | all (==Nothing) ps -> M.insert f (psls ++ [(ps,ls)]) funlabs --- default case last
    Just psls | otherwise          -> M.insert f ((ps,ls):psls) funlabs   -- special case first
    _                              -> M.insert f [(ps,ls)] funlabs

  pFunLabel x = case break (=='>') x of
    (f,_:l) -> (Just (mkCId f), l)
    _ -> (Nothing, x)

  getMaybeFun x = case x of
    "_" -> Nothing
    _ -> Just (mkCId x)

addMissing env = env {
  absLabels = (absLabels env){
    funLabels =
      foldr (\ (k,v) m -> M.insert k v m) (funLabels (absLabels env))
        [(f,[([Nothing],[head_Label])]) | 
          (f,(_,[_])) <- pgf2functions (pgfGrammar env),
          Nothing <- [M.lookup f (funLabels (absLabels env))]
          ]
       },
    cncLabels = (cncLabels env){
      morphoLabels =
        foldr (\ (k,v) m -> M.insert k v m) (morphoLabels (cncLabels env))
         [((c,0),[]) |
            ex@(c,tfs) <- lexcatTables (pgfGrammar env) (actLanguage env),
            length tfs == 1,
            Nothing <- [M.lookup (c,0) (morphoLabels (cncLabels env))]
        ]
     }
   }
 
-- #macro PredCop np cop comp : NP -> Cop -> Comp -> Cl = PredVP np (UseComp comp) ; subj cop head
-- CId (AbsType,(([CId],AbsTree),[Label]))
pMacroFunction (f:ws) = case break (==":") ws of
  (xs,_:ww) -> case break (=="=") ww of
    (ty,_:tl) -> case break (==";") tl of
      (df,_:ls) -> (pAbsType (unwords ty), ((map mkCId xs, pAbsTree (unwords df)),map labelAndMorpho ls))
      _ -> error $ "missing labels in #macro " ++ unwords (f:ws)
    _ -> error $ "missing definition in #macro " ++ unwords (f:ws)
  _ -> error $ "missing type in #macro " ++ unwords (f:ws)

inserts kvs mp = foldr (\ (k,v) m -> M.insert k v m) mp kvs

pCncLabels :: String -> CncLabels
pCncLabels = dispatch . map words . uncomment . lines
 where
  dispatch = foldr add initCncLabels
  add ws labs = case ws of
    "#morpho"  :cs:i:"_":_  | all isDigit i -> labs{morphoLabels = inserts [((mkCId c,read i),[]::[UDData]) | c <- getSeps ',' cs] (morphoLabels labs)}
    "#morpho"  :cs:i:p:_  | all isDigit i -> labs{morphoLabels = inserts [((mkCId c,read i),(prs p)::[UDData]) | c <- getSeps ',' cs] (morphoLabels labs)}
    "#word"    :w:l:"_":_ -> labs{wordLabels   = M.insert w (l,[]) (wordLabels labs)}
    "#word"    :w:l:m:_ -> labs{wordLabels   = M.insert w (l,prs m) (wordLabels labs)}
    "#lemma"   :w:l:c:p:t:_ -> labs{lemmaLabels  = inserts [((mkCId f,l),(mkCId c,(p,t))) | f <- getSeps ',' w] (lemmaLabels labs)}
    "#discont" :c:h:ps    -> labs{discontLabels = inserts
                               ([((mkCId c,i),(x_POS,head_Label,root_Label)) | is:"head":_ <- [getSeps ',' h], i <- readRange is] ++ -- bogus pos and target, to be thrown away
                                [((mkCId c,read i),(pos,lab,hd))                | p <- ps, i:pos:lab:hd:_ <- [getSeps ',' p]])
                                   (discontLabels labs)}
    "#multiword":c:hp:lab:_  -> labs{multiLabels = M.insert (mkCId c) (hp/="head-last",lab) (multiLabels labs)}
    "#auxcat":c:p:[]    -> labs{auxCategories = M.insert (mkCId c) p (auxCategories labs)}
    "#change":c1:">":c2:ws  -> labs{changeLabels = M.insert c1 [(c2, pCondition ws)] (changeLabels labs)}
    "#auxfun":f:typdef  -> labs{macroFunctions = M.insert (mkCId f) (pMacroFunction (f:typdef)) (macroFunctions labs)}
    "#disable":fs       -> labs{disabledFunctions = inserts [(mkCId f,()) | f <- fs] (disabledFunctions labs)}
    "#altfun":f:xs      -> labs{altFunLabels = M.insertWith (++) (mkCId f) [xs] (altFunLabels labs)}

    _ -> labs --- ignores silently

  readRange s = case break (=='-') s of
    (a@(_:_),_:b@(_:_)) | all isDigit a && all isDigit b -> [read a .. read b]
    _ -> error $ "no valid numeric range from " ++ s

  pCondition ws = case ws of
    "above":d:_ -> CAbove d
    "features":fs -> CFeatures (prs (unwords fs))
    _ -> error $ "cannot parse #change condition " ++ unwords ws

uncomment :: [String] -> [String]
uncomment = filter (not . all isSpace)  . map uncom
 where
  uncom cs = case cs of
    '-':'-':_ -> ""
    c:cc -> c:uncom cc
    _ -> cs
    

--------------------------
-- interfacing with GF
--------------------------

-- valcat, [argcat * label]
type LabelledType = (Cat,[(Cat,(Label,[UDData]))])  -- UDData says that certain morpho parameters must be present

mkLabelledType :: Type -> [String] -> LabelledType
mkLabelledType typ labs = case unType typ of
  (hypos,cat,_) -> (cat, zip [valCat ty | (_,_,ty) <- hypos] (map labelAndMorpho labs))
 where
  valCat ty = case unType ty of
    (_,cat,_) -> cat

labelAndMorpho :: String -> (Label,[UDData])
labelAndMorpho s = case break (=='[') s of  -- obj[Num=Sing]
    (l,_:m) -> (l, prs (init m))
    _ -> (s,[])

isEndoType, isExoType :: LabelledType -> Bool
isEndoType labtyp@(val,args) = elem val (map fst args)
isExoType = not . isEndoType

catsForPOS :: UDEnv -> M.Map POS [Either (Cat,Bool) Cat]
catsForPOS env = M.fromListWith (++) $ 
  [(p,[Left  (c,b)]) | (c,(p,b)) <- M.assocs (catLabels (absLabels env))] ++
  [(p,[Right c]) | (c, p) <- M.assocs (auxCategories (cncLabels env))]


----------------------------------------
-- special applications of annotations
----------------------------------------

-- CId (AbsType,(([CId],AbsTree),[Label]))
expandMacro :: UDEnv -> AbsTree -> AbsTree
expandMacro env tr@(RTree f ts) = case M.lookup f (macroFunctions (cncLabels env)) of
  Just (_,((xx,df),_)) -> subst (zip xx (map (expandMacro env) ts)) df
  _ -> RTree f (map (expandMacro env) ts)
 where
   subst xts t@(RTree h us) = case us of
     [] -> maybe t id (lookup h xts)
     _  -> RTree h (map (subst xts) us)

----------------------------------------------------------------------------
-- used in ud2gf: macros + real abstract functions, except the disabled ones

allFunsEnv :: UDEnv -> [(Fun,LabelledType)]
allFunsEnv env =
    [(f,(val,zip args ls))  |
      (f,((val,args),((xx,df),ls))) <- M.assocs (macroFunctions (cncLabels env))]
     ++
    [(f, mkLabelledType typ labels) |
      (f,labelss) <- M.assocs (funLabels (absLabels env)),
                    M.notMember f (disabledFunctions (cncLabels env)),
                    not (isBackupFunction f), ---- apply backups only later
      Just typ   <- [functionType (pgfGrammar env) f],
                    (_,labels) <- labelss ---- TODO precise handling of generalized labels 
    ]
     ++
    [(f, mkLabelledType typ labels) |
      (f,labelss) <- M.assocs (altFunLabels (cncLabels env)),
      labels      <- labelss,
      Just typ    <- [functionType (pgfGrammar env) f]
    ]

mkBackup ast cat = RTree (mkCId (showCId cat ++ "Backup")) [ast]
isBackupFunction f = isSuffixOf "Backup" (showCId f)


{-
data CncLabels = CncLabels {
  wordLabels    :: M.Map String (String,[UDData]),         -- word -> (lemma,morpho)          e.g. #word been be Tense=Past|VerbForm=Part
  lemmaLabels   :: M.Map (Fun,String) (Cat,(Label,Label)), -- (fun,lemma) -> (auxcat,(label,targetLabel)), e.g. #lemma UseComp be Cop cop head
  morphoLabels  :: M.Map (Cat,Int) [UDData],               -- (cat,int) -> morphotag,              e.g. #morpho V,V2,VS 0 VerbForm=Inf
  discontLabels :: M.Map (Cat,Int) (POS,Label,Label),      -- (cat,field) -> (pos,label,target)    e.g. #discont  V2  5,ADP,case,obj   6,ADV,advmod,head
  multiLabels   :: M.Map Cat (Bool, Label)                 -- cat -> (if-head-first, other-labels) e.g. #multiword Prep head first fixed
  }

data UDEnv = UDEnv {
  udFormat    :: String, -- default .conllu
  absLabels   :: AbsLabels,
  cncLabels   :: CncLabels,
  pgfGrammar  :: PGF,
  actLanguage :: Language,
  startCategory :: PGF.Type
  }
-}

checkCncLabels :: UDEnv -> CncLabels -> [String]
checkCncLabels env cls = 
  ["syncat word not covered: " ++ w |
      (w,_) <- syncats,
      Nothing <- [M.lookup w (wordLabels cls)]
  ] ++
  ["lemma not covered: " ++ w ++ " @ " ++ unwords (map showCId mfs) |
     (wf,funs) <- syncats,
     w <- nub [w | Just (w,_) <- [M.lookup wf (wordLabels cls)]],
     let mfs = [f | f <- funs, Nothing <- [lookFunLemma f w]],
     not (null mfs)
  ] ++
  ["morpho mapping missing:\n" ++ showLexcatTable ex1 |
     ex@(c,tfs) <- lexcats,
     let ms = [i | (i,tf) <- tfs, Nothing <- [M.lookup (c,i) (morphoLabels cls)]],
     not (null ms),
     let ex1 = (c, [tfs !! i | i <- ms])
    ]
  
 where
    syncats = syncatWords (pgfGrammar env) (actLanguage env)
    lookFunLemma = lookupFunLemma env (actLanguage env)
    lexcats = lexcatTables (pgfGrammar env) (actLanguage env)

lookupFunLemma env lang f w = case M.lookup (f,w) labels of
  Just r -> Just r
  _ -> M.lookup (mkCId "DEFAULT_",w) labels --- brittle to use this function name
 where
  labels = lemmaLabels (cncLabels env)

---------------------------------------------------
--- find syncategorematic words

syncatWords :: PGF -> Language -> [(String,[Fun])]
syncatWords pgf eng =
  let
    morpho = buildMorpho pgf eng
    fullform = fullFormLexicon morpho
    cfuns = [f | f <- functions pgf, Just ty <- [functionType pgf f], (_:_,_,_) <- [unType ty]]
    cfunset = S.fromList cfuns 
    isCfun f = S.member f cfunset
    syncats = [(s,[f]) | (s,fas) <- fullform, (f,a) <- fas, isCfun f]
    syncatmap = M.fromListWith (++) syncats
  in
    [(s,nub fs) | (s,fs) <- M.assocs syncatmap]


lexcatTables :: PGF -> Language -> [(Cat,[(Int,(String,String))])]
lexcatTables pgf eng =
  let
    lcatfuns = [(c,f) |   -- cat and its first function that takes no arguments
        c <- categories pgf, f:_ <- [generateAllDepth pgf (mkType [] c []) (Just 0)]]
    example f = tabularLinearizes pgf eng f
  in
    [(c,zip [0..] ts) | (c,f) <- lcatfuns, ts:_ <- [example f]]

showLexcatTable :: (Cat,[(Int,(String,String))]) -> String
showLexcatTable (c,tfs) = unlines [
  unwords ["#morpho",showCId c, show i,"--",f,w] |
    (i,(f,w)) <- tfs
    ]

