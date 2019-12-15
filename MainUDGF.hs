module Main where

import qualified UD2GF as U 
import qualified GF2UD as G
import UDAnnotations
import UDOptions

import PGF

main = do
  putStrLn $ unlines [
    "Usage: open in ghci and test with ",
    "  env <- getEnv pref eng utt",
    "  ud2gfTest env <File.conllu>",
    "  gf2udTest env <quotedSentence>",
    "  roundtrip env <quotedSentence>"
    ]

pref = "grammars/MiniLang"
--pref = "grammars/Structures"
eng = "Eng"
utt = "Utt"

ud2gf :: UDEnv -> FilePath -> IO ()
ud2gf env = ud2gfTest defaultOpts env

ud2gfTest :: Opts -> UDEnv -> FilePath -> IO ()
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
