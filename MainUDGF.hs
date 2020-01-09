module Main where

import qualified UD2GF as U 
import qualified GF2UD as G
import UDAnnotations
import UDOptions
import UDConcepts
import UDVisualization

import PGF

import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad

main = do
  xx <- getArgs
  case xx of
  
    "eval":micmac:luas:goldf:testf:_ -> do
    
      putStrLn (unwords ("evaluating": tail xx))
      
      let mcro = case micmac of "macro" -> False ; "micro" -> True ; _ -> error ("expected micro|macro, got " ++ micmac)
      let crit = case luas of "LAS" -> agreeLAS ; "UAS" -> agreeUAS ; _ -> error ("expected LAS|UAS, got " ++ luas)
      
      gold <- parseUDFile goldf
      test <- parseUDFile testf

      let score = udCorpusScore mcro crit gold test
      print score
      
    dir:path:lang:cat:opts | elem dir ["-ud2gf","-gf2ud","-string2gf2ud"] -> do
      env <- getEnv path lang cat
      convertGFUD dir (selectOpts opts) env
    _ -> putStrLn $ helpMsg

helpMsg = unlines $ [
    "Usage:",
    "   gfud (-ud2gf|-gf2ud|-string2gf2ud) <path> <language> <startcat>",
    " | gfud eval (micro|macro) (LAS|UAS) <goldfile> <testablefile>",
    "where path = grammardir/abstractprefix, language = concretesuffix",
    "The input comes from stdIO, and the output goes there as well",
    "For more functionalities: open in ghci.",
    "Options:"
    ] ++ [opt ++ "\t" ++ msg | (opt,msg) <- fullOpts]

convertGFUD :: String -> Opts -> UDEnv -> IO ()
convertGFUD dir opts env = case dir of
  "-ud2gf" -> getContents >>= ud2gfOpts (if null opts then defaultOptsUD2GF else opts) env
  "-ud2gfpar" -> getContents >>= ud2gfOptsPar (if null opts then defaultOptsUD2GF else opts) env
  _ -> do
      s <- getContents
      let conv = case dir of
            "-gf2ud" -> G.testTreeString
            "-string2gf2ud" -> G.testString
      let os = if null opts then defaultOptsGF2UD else opts
      uds <- mapM (\ (i,s) -> conv i os env s) $ zip [1..] . filter (not . null) $ lines s
      if isOpt opts "vud" then (visualizeUDSentences env uds) else return ()
   

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


-------------------

ud2gfOptsPar :: Opts -> UDEnv -> String -> IO ()
ud2gfOptsPar opts env string = do
  let eng = actLanguage env
  let sentences = map prss $ stanzas $ lines string
  let chunks = map (:[]) sentences ---- splits (length sentences `div` 8) sentences
  rs <- manyLater (mapM (U.showUD2GF opts env)) chunks
  return ()

splits n xs = case splitAt n xs of
  (x1,[])  -> [x1]
  (x1,xs2) -> x1 : splits n xs2

manyLater :: (a -> IO b) -> [a] -> IO [b]
manyLater f chunks = do
  vs <- forM chunks $ \chunk -> do
    v <- newEmptyMVar
    forkIO $ do
      x <- f chunk
      x `seq` putMVar v x
    return v
  mapM takeMVar vs

-- Build with -threaded -rtsopts
-- Run with +RTS -N$cores -RTS

--main = do
--  manyLater (print . fib) (replicate 4 35)
