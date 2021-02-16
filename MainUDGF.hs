module Main where

import qualified UD2GF as U 
import qualified GF2UD as G
import qualified DBNF as D
import UDAnnotations
import UDOptions
import UDConcepts
import GFConcepts (pAbsTree)
import UDVisualization
import UDAnalysis
import UDPatterns

import PGF

import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad
import Data.List(sortOn)

-- to get parallel processing:
-- Build with -threaded -rtsopts
-- Run with +RTS -N$cores -RTS

main = do
  xx <- getArgs
  case xx of

    "dbnf":grammarfile:startcat:opts -> D.processRuleBased grammarfile startcat opts

    "conll2latex":_ -> getContents >>= putStrLn . ud2latex . parseUDText
  
    "conll2pdf":_ -> getContents >>= visualizeUDSentences . parseUDText
  
    "check-treebank":_ -> getContents >>= putStrLn . checkUDSentences . parseUDText

    "check-annotations":path:lang:cat:_ -> checkAnnotations path lang cat
    
    "statistics":opts -> getContents >>= mapM_ print . udFrequencies (selectOpts opts) . parseUDText

    "pattern-match":ws -> do
       patterntext <- case ws of
         "-f":file:_ -> readFile file
         _ -> return $ unwords ws
       let pattern = (read patterntext) :: UDPattern
       ss <- getContents >>= return . parseUDText
       mapM_ putStrLn $ filter (not . null) $ map (showMatchesInUDSentence pattern) ss
    
    "pattern-replace":ws -> do
       patterntext <- case ws of
         "-f":file:_ -> readFile file
         _ -> return $ unwords ws
       let pattern = (read patterntext) :: UDReplacement
       ss <- getContents >>= return . parseUDText
       mapM_ putStrLn $ map (showReplacementsInUDSentence pattern) ss
    
    "cosine-similarity":file1:file2:opts -> do
      ud1 <- parseUDFile file1
      ud2 <- parseUDFile file2
      print $ udCosineSimilarity (selectOpts opts) ud1 ud2
  
    "not-covered":file1:file2:opts -> do
      ud1 <- parseUDFile file1
      ud2 <- parseUDFile file2
      putStrLn $ unwords $ map show $ notCoveredFeatures (selectOpts opts) ud1 ud2
  
    "parse2latex":file:_ -> getContents >>= absTrees2latex initUDEnv file . map pAbsTree . selectParseTrees . lines
    
    "parse2pdf":_ -> getContents >>= visualizeAbsTrees initUDEnv . map pAbsTree . selectParseTrees . lines

    "extract-pos-words":_ -> getContents >>= putStrLn . unlines . map ud2poswords . parseUDText
    "extract-pos-feats-words":_ -> getContents >>= putStrLn . unlines . map ud2posfeatswords . parseUDText

    "lexical-entries":annots:_ -> do
       env <- getAnnotEnv annots
       uds <- getContents >>= return . parseUDText
       let entries = lexicalEntries env uds
       putStrLn $ unlines [unwords [w ++ "_" ++ c, c, "--", show i] | ((w,c),i) <- entries]

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
      
    dir:path:lang:cat:opts | elem (dropWhile (=='-') dir) ["ud2gf","gf2ud","ud2gfpar","string2gf2ud"] -> do
      env <- getEnv path lang cat
      convertGFUD (dropWhile (=='-') dir) (selectOpts opts) env
    _ -> putStrLn $ helpMsg

helpMsg = unlines $ [
    "Usage:",
    "   gfud (ud2gf|gf2ud|string2gf2ud|gf2udpar) <path> <language> <startcat>",
    " | gfud dbnf <dbnf-grammarfile> <startcat> <-cut=NUMBER>? <-show=NUMBER>? <-onlyparsetrees>?",
    " | gfud eval (micro|macro) (LAS|UAS) <goldfile> <testablefile> units?",
    " | gfud check-treebank",
    " | gfud check-annotations <path> <language> <startcat>",
    " | gfud statistics <option>*",
    " | gfud pattern-match ('<pattern>' | -f <file>)",
    " | gfud pattern-replace ('<replacement>' | -f <file>)",
    " | gfud cosine-similarity <file> <file> <option>*",
    " | gfud not-covered <standardfile> <testedfile> <option>*",
    " | gfud extract-pos-words",
    " | gfud extract-pos-feats-words",
    " | gfud lexical-entries <abslabels-file>",
    " | gfud conll2pdf",
    " | gfud parse2pdf",
    " | gfud conll2latex",
    " | gfud parse2latex <file>",
    "where path = grammardir/abstractprefix, language = concretesuffix",
    "Except for file arguments, the input comes from stdIO, and the output goes there as well",
    "The option -gf2udpar should be used with the Haskell runtime flag +RTS -Nx -RTS",
    "where x is the number of cores you want to use in parallel processing.",
    "For more functionalities: open in ghci.",
    "Pattern syntax:" ,
    "   (FORM | LEMMA | POS | DEPREL | DEPREL_) <string>",
    " | ARG <pos> <deprel>",
    " | (FEATS | FEATS_) <features>",
    " | (AND | OR) [ <pattern>,* ]",
    " | (SEQUENCE | SEQUENCE_) [ <pattern>,* ]",
    " | NOT <pattern>",
    " | (TREE | TREE_) <pattern> <pattern>*",
    " | (DEPTH_EQUALS | DEPTH_UNDER | DEPTH_OVER) <int>",
    " | TRUE",
    "where DEPREL_, FEATS_, SEQUENCW_, TREE_ mean matching a subset/substring.",
    "<string> arguments require double quotes, and the <pattern> itself is in single quotes",
    "if read from command line, but not if read from a file (the -f option).",
    "Replacement syntax:",
    "   REPLACE <pattern> <pattern>",
    " | UNDER <pattern> <replacement>",
    " | PRUNE <pattern> <int>",
    " | FILTER_SUBTREES <pattern> <pattern>",
    " | FLATTEN <pattern>",
    " | RETARGET <pattern> <pattern> <pattern>",
    " | CHANGES [ <replacement>,* ]",
    " | COMPOSE [ <replacement>,* ]",
    "Options:"
    ] ++ ["  " ++ opt ++ "\t" ++ msg | (opt,msg) <- fullOpts]

convertGFUD :: String -> Opts -> UDEnv -> IO ()
convertGFUD dir opts env = case dir of
  "ud2gf" -> getContents >>= ud2gfOpts (if null opts then defaultOptsUD2GF else opts) env
  "ud2gfpar" -> getContents >>= ud2gfOptsPar (if null opts then defaultOptsUD2GF else opts) env
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
   

ud2gf :: UDEnv -> String -> IO ()
ud2gf = ud2gfOpts defaultOptsUD2GF 

ud2gfOpts :: Opts -> UDEnv -> String -> IO ()
ud2gfOpts opts env = U.test opts env

gf2ud :: UDEnv -> String -> IO ()
gf2ud = gf2udOpts defaultOptsGF2UD 

gf2udOpts :: Opts -> UDEnv -> String -> IO ()
gf2udOpts opts env s = G.testString 1 opts env s >> return ()

roundtripOpts :: Opts -> Opts -> UDEnv -> String -> IO ()
roundtripOpts gopts uopts env s = do
  putStrLn "FROM GF"
  u <- G.testString 1 gopts env s
  putStrLn "FROM UD BACK TO GF"
  U.showUD2GF uopts env u
  return ()

roundtrip :: UDEnv -> String -> IO ()
roundtrip = roundtripOpts defaultOptsGF2UD (selectOpts ["dt1","bt","at0","at","tc","lin"])

-- for quick use in ghci
mini = "grammars/MiniLang"
shallow = "grammars/ShallowParse"
term = "grammars/Term"
eng = "Eng"
utt = "Utt"
termInfix = "Infix"
termcat = "Term"

checkUDSentences :: [UDSentence] -> String
checkUDSentences uds = unlines $
  errorsInUDSentences uds

-------------------

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
