module Main where

---import qualified UD2GF as U 
---import qualified GF2UD as G
import qualified DBNF as D
import UDAnnotations
import UDOptions
import UDConcepts
import GFConcepts (pAbsTree)
---import UDVisualization
import UDAnalysis
import UDPatterns
import RTree

---import PGF

import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad
import Data.List(sortOn, (\\))
import Data.Char(isDigit)

-- to get parallel processing:
-- Build with -threaded -rtsopts
-- Run with +RTS -N$cores -RTS

main = do
  xx <- getArgs
  case xx of

    "dbnf":grammarfile:startcat:opts -> D.processRuleBased grammarfile startcat opts

---    "conll2latex":_ -> getContents >>= putStrLn . ud2latex . parseUDText
  
---    "conll2pdf":_ -> getContents >>= visualizeUDSentences . parseUDText
  
    "check-treebank":_ -> getContents >>= putStrLn . checkUDSentences . parseUDText

    "check-annotations":path:lang:cat:_ -> checkAnnotations path lang cat
    
    "statistics":opts -> getContents >>= mapM_ print . udFrequencies (selectOpts opts) . parseUDText

    "pattern-match":ws0 -> do
       let (opts,ws) = span (flip elem ["adjust","prune"]) ws0
       let sopts = selectOpts opts
       patterntext <- case ws of
         "-f":file:_ -> readFile file
         _ -> return $ unwords ws
       let pattern = (read patterntext) :: UDPattern
       ss <- getContents >>= return . parseUDText
       mapM_ putStrLn $ filter (not . null) $ map (showMatchesInUDSentence sopts pattern) ss
    
    "pattern-replace":ws -> do
       patterntext <- case ws of
         "-f":file:_ -> readFile file
         _ -> return $ unwords ws
       let pattern = (read patterntext) :: UDReplacement
       putStrLn $ "## " ++ show pattern
       ss <- getContents >>= return . parseUDText
       mapM_ putStrLn $ map (showReplacementsInUDSentence pattern) ss
    
    "cosine-similarity":file1:file2:opts -> do
      ud1 <- parseUDFile file1
      ud2 <- parseUDFile file2
      print $ udCosineSimilarity (selectOpts opts) ud1 ud2
  
    "cosine-similarity-sort":file1:file2:fopts -> do
      ud1 <- parseUDFile file1
      ud2 <- parseUDFile file2
      let (limit,opts) = case fopts of
             "-threshold":d:ropts | all isDigit d -> ((read d :: Double)/100, selectOpts ropts)
             _ -> (0, selectOpts fopts)
      let reference = udFrequencyMap opts ud1
      let results = [(ud,sim) |
            ud <- ud2,
            let udmap = udFrequencyMap opts [ud],
            let sim = cosineSimilarityOfMaps reference udmap
            ]
      let sortedResults = sortOn ((0-) . snd) (filter ((>= limit) . snd) results)
      flip mapM_ sortedResults $ \ (ud,sim) -> do
        putStrLn $ prt $ ud{udCommentLines = ("# similarity " ++ show sim) : udCommentLines ud}
  
    "not-covered":file1:file2:opts -> do
      ud1 <- parseUDFile file1
      ud2 <- parseUDFile file2
      putStrLn $ unwords $ map show $ notCoveredFeatures (selectOpts opts) ud1 ud2
  
---    "parse2latex":file:_ -> getContents >>= absTrees2latex initUDEnv file . map pAbsTree . selectParseTrees . lines
    
---    "parse2pdf":_ -> getContents >>= visualizeAbsTrees initUDEnv . map pAbsTree . selectParseTrees . lines

    "conll2tree":_ -> getContents >>= mapM_ putStrLn . map (prUDTree . udSentence2tree) . parseUDText
    "adjust-positions":_ -> getContents >>= mapM_ putStrLn . map (prt . udTree2sentence . createRoot . udSentence2tree . adjustUDIds) . parseUDText

    "conll2reduced":patt:_ -> getContents >>= mapM_ putStrLn . map (prReducedUDSentence patt) . parseUDText
    "reduced2conll":patt:_ -> getContents >>= mapM_ (putStrLn . prt) . map (pReducedUDSentence patt) . stanzas . lines
    "oneliner2conll":patt:_ -> getContents >>= mapM_ (putStrLn . prt) . map (pOneLineUDSentence patt) . lines
    
    "extract-pos-words":_ -> getContents >>= putStrLn . unlines . map ud2poswords . parseUDText
    "extract-pos-feats-words":_ -> getContents >>= putStrLn . unlines . map ud2posfeatswords . parseUDText

    "sample":n:_ -> getContents >>= mapM_ (putStrLn . prt) . sampleFromList (read n) . parseUDText
    "first":n:_ -> getContents >>= mapM_ (putStrLn . prt) . take (read n) . parseUDText
    "last":n:_ -> getContents >>= mapM_ (putStrLn . prt) . takeLast (read n) . parseUDText

    "immediate-subtrees":_ -> getContents >>= mapM_ (putStrLn . prt . udTree2sentence . adjustRootAndPositions) . concatMap (subtrees . udSentence2tree) . parseUDText

    "extract-dbnf":n:_ -> getContents >>= putStrLn . extractDBNF (read n)
    
    "calibrate-dbnf":tbfile:_ -> do
      tb  <- parseUDFile tbfile
      dbnf <- getContents >>= return . D.pGrammar
      let cdbnf = calibrateDBNF tb dbnf
      putStrLn $ D.prGrammar cdbnf

    
    "lexical-entries":annots:_ -> do
       env <- getAnnotEnv [annots]
       uds <- getContents >>= return . parseUDText
       let entries = lexicalEntries env uds
       putStrLn $ unlines [unwords [w ++ "_" ++ c, "--", show (reverse i)] | ((w,c),i) <- entries]
       
    "lexical-entries-gf":annots:pgf:_ -> do
       env <- getAnnotEnv [annots,pgf]
       uds <- getContents >>= return . parseUDText
       let entries = lexicalEntriesGF env uds
       putStrLn $ unlines entries

    "eval":micmac:luas:goldf:testf:opts -> do
    
      putStrLn (unwords ("evaluating": tail xx))
      
      let mcro = case micmac of "macro" -> False ; "micro" -> True ; _ -> error ("expected micro|macro, got " ++ micmac)
      let crit = case luas of "LAS" -> agreeLAS ; "UAS" -> agreeUAS ; _ -> error ("expected LAS|UAS, got " ++ luas)
      
      gold <- parseUDFile goldf
      test <- parseUDFile testf

      case opts of
        _ | isOpt (selectOpts opts) "units" -> do
          let pairs = zip gold test
          let scores = sortOn (udScore . fst) [(snd (udSentenceScore crit go [te]), (go,te)) | (go,te) <- pairs]
          flip mapM_ scores $ \ (score,(go,te)) -> do
            print score
            putStrLn $ prUDAlign go te
          
        _ -> do
          let score = udCorpusScore mcro crit gold test
          print score
      
---    dir:path:lang:cat:opts | elem (dropWhile (=='-') dir) ["ud2gf","gf2ud","ud2gfparallel","string2gf2ud"] -> do
---      env <- getEnv path lang cat
---      convertGFUD (dropWhile (=='-') dir) (selectOpts opts) env
    _ -> putStrLn $ helpMsg

helpMsg = unlines $ [
    "Usage:",
    "   gfud check-treebank",
    " | gfud statistics <option>*",
    " | gfud pattern-match (adjust|prune)? ('<pattern>' | -f <file>)",
    " | gfud pattern-replace ('<replacement>' | -f <file>)",
    " | gfud cosine-similarity <file> <file> <option>*",
    " | gfud cosine-similarity-sort <file> <file> <option>*",
    " | gfud not-covered <standardfile> <testedfile> <option>*",
    " | gfud extract-pos-words",
    " | gfud extract-pos-feats-words",
    " | gfud lexical-entries <abslabels-file>",
    " | gfud lexical-entries-gf <abslabels-file> <morphodict-pgf-file>",
    " | gfud sample <int>",
    " | gfud first <int>",
    " | gfud last <int>",
    " | gfud immediate-subtrees",
    " | gfud conll2tree",
    " | gfud conll2reduced <reduce_pattern>",
    " | gfud reduced2conll <reduce_pattern>",
    " | gfud oneliner2conll <reduce_pattern>",
    " | gfud adjust-positions",
    " | gfud conll2pdf",
    " | gfud parse2pdf",
    " | gfud conll2latex",
    " | gfud parse2latex <file>",
    " | gfud eval (micro|macro) (LAS|UAS) <goldfile> <testablefile> units?",
    " | gfud dbnf <dbnf-grammarfile> <startcat> <-cut=NUMBER>? <-show=NUMBER>? <-onlyparsetrees>?",
    " | gfud extract-dbnf <int>",
    " | gfud calibrate-dbnf <file>",
    " | gfud check-annotations <path> <language> <startcat>",
    " | gfud (ud2gf|gf2ud|string2gf2ud|ud2gfparallel) <path> <language> <startcat> <option>*",
    "where path = grammardir/abstractprefix, language = concretesuffix.",
    "The files read are <path>.pgf, <path>.labels, and <path><language>.labels.",
    "Except for <file> arguments, the input comes from stdIO, and the output goes there as well",
    "The option ud2gfparallel should be used with the Haskell runtime flag +RTS -Nx -RTS",
    "where x is the number of cores you want to use in parallel processing.",
    "For more functionalities: open MainUDGF.hs in ghci.",
    "Pattern syntax:" ,
    "   (FORM | LEMMA | POS | DEPREL | DEPREL_ | FEATS | FEATS_) <string>",
    " | ARG <pos> <deprel>",
    " | (AND | OR) [ <pattern>,* ]",
    " | (SEQUENCE | SEQUENCE_) [ <pattern>,* ]",
    " | NOT <pattern>",
    " | (TREE | TREE_) <pattern> <pattern>*",
    " | (DEPTH | DEPTH_UNDER | DEPTH_OVER) <int>",
    " | (LENGTH | LENGTH_UNDER | LENGTH_OVER) <int>",
    " | TRUE",
    " | PROJECTIVE",
    "where DEPREL_, FEATS_, SEQUENCW_, TREE_ mean matching a subset/substring.",
    "<string> arguments require double quotes, and the <pattern> itself is in single quotes",
    "if read from command line, but not if read from a file (the -f option).",
    "Replacement syntax:",
    "   (REPLACE_FORM | REPLACE_LEMMA | REPLACE_POS | REPLACE_DEPREL | REPLACE_DEPREL_) <string> <string>",
    " | (REPLACE_FEATS | REPLACE_FEATS_) <string> <string>",
    " | IF <pattern> <replacement>",
    " | UNDER <pattern> <replacement>",
    " | OVER <pattern> <replacement>",
    " | PRUNE <pattern> <int>",
    " | FILTER_SUBTREES <pattern> <pattern>",
    " | FLATTEN <pattern>",
    " | RETARGET <pattern> <pattern> <pattern>",
    " | CHANGES [ <replacement>,* ]",
    " | COMPOSE [ <replacement>,* ]",
    "<reduce_pattern> is a string such as xx_x__xx that hides the _ fields of UD words in conll2reduced.",
    "In reduced2conll it inserts _ to those fields to obtain legal CoNLL.",
    "In oneliner2conll xx_x__xx, each sentence is given as in '1 John NOUN 2 nsubj ; 2 walks VERB 0 root'.",
    "",
    "Options:"
    ] ++ ["  " ++ opt ++ "\t" ++ msg | (opt,msg) <- fullOpts]

{- ---
convertGFUD :: String -> Opts -> UDEnv -> IO ()
convertGFUD dir opts env = 
  let optsU2G = if null (opts \\ nonPrintingOpts) then opts ++ defaultOptsUD2GF else opts
  in case dir of
    "ud2gf" -> getContents >>= ud2gfOpts optsU2G env
    "ud2gfparallel" -> getContents >>= ud2gfOptsPar optsU2G env
    _ -> do
      s <- getContents
      let conv = case dir of
            "gf2ud" -> G.testTreeString
            "string2gf2ud" -> G.testString
      let os = if null opts then defaultOptsGF2UD else opts
      uds <- mapM (\ (i,s) -> conv i os env s) $ zip [1..] . filter (not . null) $ lines s
      case opts of
        _ | isOpt opts "lud" -> putStrLn $ ud2latex uds
        _ | isOpt opts "vud" -> visualizeUDSentences uds
        _ -> return ()
-}

---ud2gf :: UDEnv -> String -> IO ()
---ud2gf = ud2gfOpts defaultOptsUD2GF 

---ud2gfOpts :: Opts -> UDEnv -> String -> IO ()
---ud2gfOpts opts env = U.test opts env

---gf2ud :: UDEnv -> String -> IO ()
---gf2ud = gf2udOpts defaultOptsGF2UD 

---gf2udOpts :: Opts -> UDEnv -> String -> IO ()
---gf2udOpts opts env s = G.testString 1 opts env s >> return ()

---roundtripOpts :: Opts -> Opts -> UDEnv -> String -> IO ()
---roundtripOpts gopts uopts env s = do
---  putStrLn "FROM GF"
---  u <- G.testString 1 gopts env s
---  putStrLn "FROM UD BACK TO GF"
---  U.showUD2GF uopts env u
---  return ()

---roundtrip :: UDEnv -> String -> IO ()
---roundtrip = roundtripOpts defaultOptsGF2UD (selectOpts ["dt1","bt","at0","at","tc","lin"])

-- for quick use in ghci
mini = "grammars/MiniLang"
shallow = "grammars/ShallowParse"
term = "grammars/Term"
eng = "Eng"
utt = "Utt"
termInfix = "Infix"
termcat = "Term"

checkUDSentences :: [UDSentence] -> String
checkUDSentences uds = case errorsInUDSentences uds of
  [] -> "# treebank OK"
  msgs -> unlines msgs


sampleFromList n xs = take n [x | (x,i) <- zip xs [1..], mod i r == 0]
  where
    r = max 1 (div (length xs) n) 

takeLast n xs = drop (length xs - n) xs

{-
-------------------
---
ud2gfOptsPar :: Opts -> UDEnv -> String -> IO () --- [AbsTree]
ud2gfOptsPar opts env string = do
  let eng = actLanguage env
  let sentences = map prss $ stanzas $ lines string
  tstats <- manyLater (U.showUD2GF minimalOptsUD2GF env) sentences
  let globalStats = U.combineUD2GFStats $ map snd tstats
  ifOpt opts "stat" $ U.prUD2GFStat globalStats
---  if isOpt opts "vat" then (visualizeAbsTrees env (map expr2abstree (concatMap fst tstats))) else return ()
  return () --- return (map fst tstats)

splits n xs = case splitAt n xs of
  (x1,[])  -> [x1]
  (x1,xs2) -> x1 : splits n xs2

-- from Anton Ekblad 2020-01-09
manyLater :: (a -> IO b) -> [a] -> IO [b]
manyLater f chunks = do
  vs <- forM chunks $ \chunk -> do
    v <- newEmptyMVar
    forkIO $ do
      x <- f chunk
      x `seq` putMVar v x
    return v
  mapM takeMVar vs
-}