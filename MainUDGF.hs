module Main where

import qualified UD2GF as U 
import qualified GF2UD as G
import UDAnnotations

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

ud2gfTest :: UDEnv -> FilePath -> IO ()
ud2gfTest env = U.test env

gf2udTest :: UDEnv -> String -> IO ()
gf2udTest env s = G.test env s >> return ()

roundtrip :: UDEnv -> String -> IO ()
roundtrip env s = do
  putStrLn "FROM GF"
  u <- G.test env s
  putStrLn "BACK TO UD"
  U.showUD2GF env u
  return ()
