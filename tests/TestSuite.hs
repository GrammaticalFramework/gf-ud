module Main where

import Test.Hspec
import UD2GF
import PGF
import UDAnnotations
import UDConcepts (UDData(UDData))


myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "Test") "Eng" "UDS"
  where path x = "tests/grammars/" ++ x

main :: IO ()
main = do
  env <- myUDEnv
  -- TODO: run "gf --make TestOrderingEng.gf" to generate TestOrderingEng.pgf
  someCats <- readFile "tests/grammars/some_cats.conllu"
  tenHovercrafts <- readFile "tests/grammars/test_distance.conllu"
  hspec $ do
    describe "Prefer flat trees" $ do
      it "should pick the flatter tree of the two alternatives" $ do
        bestTree env theCatSleepsAlready `shouldBe` "root_nsubj_obl (UseV sleep_V) (DetCN the_Det (UseN cat_N)) already_Adv"
    describe "Allow matching on LEMMA" $ do
      it "should handle 'LEMMA' as a UD tag" $ do
        -- TODO: It shouldn't be using ImpVP. Fixed by not ignoring startCat (#16).
        -- It might also be worthwhile to prioritize smaller trees of different categories,
        -- instead of the current behaviour of preferring categories that are earlier in alphabetic order.
        bestTrees env someCats `shouldBe`
          ["DetCN anySg_Det (UseN cat_N)"
          ,"DetCN anyPl_Det (UseN cat_N)"
          ,"DetCN someSg_Det (UseN cat_N)"
          ,"DetCN somePl_Det (UseN cat_N)"]
    describe "Parsing for labels" $ do
      it "should allow an escaped comma as a UD tag" $ do
        labelAndMorpho "head[LEMMA=\\,]" `shouldBe` ("head", [UDData "LEMMA" [","]])
    describe "Match on DISTANCE" $ do
      it "should handle 'DISTANCE' as keyword, CG-style" $ do
        bestTrees env tenHovercrafts `shouldBe`
          ["ApposNum (UseN hovercraft_N) ten_Num"
          ,"DetCN (num2Det ten_Num) (UseN hovercraft_N)"]



bestTrees :: UDEnv -> String -> [String]
bestTrees env conll = map exprStr exprs
  where
      exprs = getExprs [] env conll
      exprStr expr = case expr of
        (x : _xs) -> showExpr [] x
        _ -> "bestTree: ud2gf failed"


bestTree :: UDEnv -> String -> String
bestTree env conll = exprStr
  where
      exprs = getExprs [] env conll
      exprStr = case exprs of
        (x : _xs) : _xss -> showExpr [] x
        _ -> "bestTree: ud2gf failed"

theCatSleepsAlready :: String
theCatSleepsAlready = unlines 
  [ "1\tthe\tthe\tDET\tQuant\tFORM=0\t2\tdet\t_\tFUN=DefArt"
  , "2\tcat\tcat\tNOUN\tNN\tNumber=Sing\t3\tnsubj\t_\tFUN=cat_N"
  , "3\tsleeps\tsleep\tVERB\tVBZ\tMood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin\t0\troot\t_\tFUN=sleepVBZ"
  , "4\talready\talready\tADV\tRB\t_\t3\tadvmod\t_\t_"
  ]
