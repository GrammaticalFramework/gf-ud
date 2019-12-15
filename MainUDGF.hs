module Main where

import qualified UD2GF as U 
import qualified GF2UD as G
import UDAnnotations
import UDOptions

import PGF

main = do
  putStrLn $ helpMsg

helpMsg = unlines $ [
    "Usage: open in ghci and test with ",
    "  env <- getEnv (mini|shallow|term) (eng|termInfix) (utt|termcat)",
    "  ud2gf env <quickUD>|<<File.conllu>",
    "  gf2ud env <quotedSentence>",
    "  ud2gfTest (selectOptions [...]) env <quickUD>|<<File.conllu>",
    "  gf2udTest (selectOptions [...]) env <quotedSentence>",
    "  roundtrip env <quotedSentence>",
    "quickUD format:",
    "  1 John John NOUN 2 nsubj ; 2 walks walk VERB 0 root",
    "Options (in quotes, separated by commas):"
    ] ++ [opt ++ "\t" ++ msg | (opt,msg) <- fullOpts]

mini = "grammars/MiniLang"
shallow = "grammars/ShallowParse"
term = "grammars/Term"
--pref = "grammars/Structures"
eng = "Eng"
utt = "Utt"
termInfix = "Infix"
termcat = "Term"

ud2gf :: UDEnv -> String -> IO ()
ud2gf = ud2gfOpts defaultOptsUD2GF 

ud2gfOpts :: Opts -> UDEnv -> String -> IO ()
ud2gfOpts opts env = U.test opts env

gf2ud :: UDEnv -> String -> IO ()
gf2ud = gf2udOpts defaultOptsGF2UD 

gf2udOpts :: Opts -> UDEnv -> String -> IO ()
gf2udOpts opts env s = G.test opts env s >> return ()

roundtripOpts :: Opts -> Opts -> UDEnv -> String -> IO ()
roundtripOpts gopts uopts env s = do
  putStrLn "FROM GF"
  u <- G.test gopts env s
  putStrLn "FROM UD BACK TO GF"
  U.showUD2GF uopts env u
  return ()

roundtrip :: UDEnv -> String -> IO ()
roundtrip = roundtripOpts defaultOptsGF2UD minimalOptsUD2GF

