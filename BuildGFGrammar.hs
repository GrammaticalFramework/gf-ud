module BuildGFGrammar where

import GFConcepts
import UDConcepts

import PGF

import qualified Data.Map as M
import System.Environment (getArgs)

testBuildGrammar =
  readFile "data/example-eng-ita-aligns.txt" >>=
  buildGFGrammar "grammars/Extract.pgf" ["grammars/MorphoDictEng.pgf", "grammars/MorphoDictIta.pgf"]

buildGFGrammar :: FilePath -> [FilePath] -> String -> IO ()
buildGFGrammar abstr dicts als = do
  env <- getGrammarEnv abstr dicts
  let aligns = readAlignments als
  let ruless = map (tree2rules env) aligns
  putStrLn $ unlines $ map prBuiltRules ruless


getGrammarEnv :: FilePath -> [FilePath] -> IO GrammarEnv
getGrammarEnv abstr dicts = do
  syntpgf <- readPGF abstr
  dictpgfs <- mapM readPGF dicts
  return $ GrammarEnv {
    absname = mkCId "Extracted",
    syntaxpgf = syntpgf,
    absbasemodules = [],
    langenvs = M.fromList [
      (lang,
       LangEnv {
         cncname = mkCId ("Extracted" ++ lang),
         dictpgf = pgf,
         basemodules = [],
         resourcemodules = [abstr ++ lang]
         }) |
              (dict,pgf) <- zip dicts dictpgfs,
              let (abstr,lang,_) = partsOfFileName dict
       ]
    }


type LangName = String

data GrammarEnv = GrammarEnv {
  absname :: CId,
  syntaxpgf :: PGF,
  absbasemodules :: [String],
  langenvs :: M.Map LangName LangEnv -- lookup with langname e.g. "Eng"
  }

data LangEnv = LangEnv {
  cncname  :: CId,
  dictpgf  :: PGF,
  basemodules :: [String],    -- to be extended
  resourcemodules :: [String] -- to be opened
  }

data BuiltRules = BuiltRules {
  funname  :: String,
  funcats  :: [(LangName,String)], -- can be different cats in different langs
  linrules :: [(LangName,String)],
  unknowns :: [(LangName,[(String,String)])] -- unknown lex item with its cat
  }
  deriving Show

tree2rules :: GrammarEnv -> [(LangName,Tree)] -> BuiltRules
tree2rules env lts = BuiltRules {
  funname = fun,
  funcats  = [(lang, showCId cat) | (lang,tree) <- lts, (cat,_) <- [valcat lang (rootfun tree)]],
  linrules = [(lang, linrule lang tree) | (lang,tree) <- lts],
  unknowns = [(lang, unknown lang tree) | (lang,tree) <- lts]
  }
 where
   fun = showCId
     (mkFun (concatMap (init . partsOfFun) (lexitems firsttree))
            (fst (valcat firstlang (rootfun firsttree))))
   
   valcat l f = case functionType synpgf f of
     Just ty -> case unType ty of
       (_,cat,_) -> (cat,0)                       -- function in syntax
     _ -> case functionType (dictpgf (envoflang l)) f of
       Just ty -> case unType ty of
         (_,cat,_) -> (cat,1)                     -- word in lexicon
       _ -> (mkCId (last (partsOfFun f)),2) -- unknown word

   unknown l t = [(showCId f, showCId c) |
     f <- lexitems t,
     (c,2) <- [valcat l f]
     ]

   linrule lang tree = showExpr [] tree
   lexitems t = leavesRTree (expr2abstree t)
   rootfun t = root (expr2abstree t)
   synpgf = syntaxpgf env
   (firstlang,firsttree) = head lts
   envoflang l = maybe (error ("unknown lang " ++ l)) id $ M.lookup l (langenvs env)

prBuiltRules br = unlines $ [
  let cat:cats = map snd (funcats br) in
    unwords ["fun",funname br,":",cat,";","--",unwords cats]
  ] ++ [
  unwords ["lin",funname br,"=",lin,";","--",lang] | (lang,lin) <- linrules br
  ] ++ [
  unwords ["oper",fun,"=","mk"++cat, word fun,";","--",lang] | (lang,funcats) <- unknowns br, (fun,cat) <- funcats
  ]
 where
   word f = "\"" ++ takeWhile (/='_') f ++ "\""
   

readAlignments :: String -> [[(LangName,Tree)]]
readAlignments = map (map getOne) . stanzas . lines
 where
   getOne line = case words line of  -- Eng: DetCN ...
     lang:tree -> (init lang, getTree (unwords tree))
   getTree s = case readExpr s of
     Just t -> t
     _ -> error ("no tree from: " ++ s)