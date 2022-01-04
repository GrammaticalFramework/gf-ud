module Main where

import Test.Hspec
import UD2GF
import PGF
import UDAnnotations


myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "TestOrdering") "Eng" "UDS"
  where path x = "tests/grammars/" ++ x

main :: IO ()
main = do
  env <- myUDEnv
  hspec $ do
    describe "Prefer flat trees" $ do
        it "should pick the flatter tree of the two alternatives" $ do
        bestTree env theCatSleepsAlready `shouldBe` "root_nsubj_obl (UseV sleep_V) (DetCN the_Det (UseN cat_N)) already_Adv"


bestTree :: UDEnv -> String -> String
bestTree env conll = exprStr
  where
      exprs = getExprs [] env conll
      exprStr = case exprs of
        (x : _xs) : _xss -> showExpr [] x
        _ -> "bestTree: ud2gf failed"

theCatSleepsAlready :: String
theCatSleepsAlready = "1\tthe\tthe\tDET\tQuant\tFORM=0\t2\tdet\t_\tFUN=DefArt\n2\tcat\tcat\tNOUN\tNN\tNumber=Sing\t3\tnsubj\t_\tFUN=cat_N\n3\tsleeps\tsleep\tVERB\tVBZ\tMood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin\t0\troot\t_\tFUN=sleepVBZ\n4\talready\talready\tADV\tRB\t_\t3\tadvmod\t_\t_"
