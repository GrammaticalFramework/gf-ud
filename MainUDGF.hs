module Main where

import qualified UD2GF as U 
import qualified GF2UD as G
import UDAnnotations
import UDOptions
import UDVisualization

import System.Environment (getArgs)

import PGF

main = do
  xx <- getArgs
  case xx of
    dir:path:lang:cat:opts | elem dir ["-ud2gf","-gf2ud","-string2gf2ud"] -> do
      env <- getEnv path lang cat
      convertGFUD dir (selectOpts opts) env
    _ -> putStrLn $ helpMsg

helpMsg = unlines $ [
    "Usage:",
    "  gfud (-ud2gf|-gf2ud|-string2gf2ud) <path> <language> <startcat>",
    "where path = grammardir/abstractprefix, language = concretesuffix",
    "The input comes from stdIO, and the output goes there as well",
    "For more functionalities: open in ghci.",
    "Options:"
    ] ++ [opt ++ "\t" ++ msg | (opt,msg) <- fullOpts]

convertGFUD :: String -> Opts -> UDEnv -> IO ()
convertGFUD dir opts env = case dir of
  "-ud2gf" -> getContents >>= ud2gfOpts (if null opts then defaultOptsUD2GF else opts) env
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
roundtrip = roundtripOpts defaultOptsGF2UD minimalOptsUD2GF

-- for quick use in ghci
mini = "grammars/MiniLang"
shallow = "grammars/ShallowParse"
term = "grammars/Term"
eng = "Eng"
utt = "Utt"
termInfix = "Infix"
termcat = "Term"
