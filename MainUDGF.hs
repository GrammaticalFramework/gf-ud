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
    "  env <- getEnv path eng utt",
    "  ud2gf env <quickUD>|<<File.conllu>",
    "  gf2ud env <quotedSentence>",
    "  ud2gfTest (selectOptions [...]) env <quickUD>|<<File.conllu>",
    "  gf2udTest (selectOptions [...]) env <quotedSentence>",
    "  roundtrip env <quotedSentence>",
    "quickUD format:",
    "  1 John John NOUN 2 nsubj ; 2 walks walk VERB 0 root",
    "Options (in quotes, separated by commas):"
    ] ++ [opt ++ "\t" ++ msg | (opt,msg) <- fullOpts]

path = "grammars/MiniLang"
bigpath = "grammars/ShallowParse"
--pref = "grammars/Structures"
eng = "Eng"
utt = "Utt"

ud2gf :: UDEnv -> String -> IO ()
ud2gf env = ud2gfTest defaultOpts env

ud2gfTest :: Opts -> UDEnv -> String -> IO ()
ud2gfTest opts env = U.test opts env

gf2ud :: UDEnv -> String -> IO ()
gf2ud env s = G.test env s >> return ()

roundtrip :: UDEnv -> String -> IO ()
roundtrip env s = do
  putStrLn "FROM GF"
  u <- G.test env s
  putStrLn "FROM UD BACK TO GF"
  U.showUD2GF (selectOpts ["ut","at"]) env u
  return ()
