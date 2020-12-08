module BuildGFGrammar where

import GFConcepts
import UDConcepts

import PGF

import qualified Data.Map as M
import System.Environment (getArgs)
import Data.List

testBuildGrammar =
  readFile "data/example-eng-ita-aligns.txt" >>=
  buildGFGrammar "grammars/Extract.pgf" ["grammars/MorphoDictEng.pgf", "grammars/MorphoDictIta.pgf"]
-- before running the test, do
--
-- $ gf -make MorphodictLang.gf -o MorphoDictLang
-- $ gf -make Extract*.gf
--
-- Then just evaluate testBuildGrammar in ghci.
-- After this, do
--
-- $ grep Abstr Extracted.tmp >Extracted.gf
-- $ grep Eng Extracted.tmp >ExtractedEng.gf
-- $ grep Ita Extracted.tmp >ExtractedIta.gf
--
-- Make sure Extract*.gf and MorphoDict*.gf are on your path in GF.
-- Then, in GF,
--
-- > import ExtractedEng.gf ExtractedIta.gf
-- > gt -cat=CN -depth=0 | l -tabtreebank
--
-- you will see some nice things! E.g.
--
-- travel_expert_esperto_di_viaggio_CN	travel expert	esperto di viaggi

buildGFGrammar :: FilePath -> [FilePath] -> String -> IO ()
buildGFGrammar abstr dicts als = do
  env <- getGrammarEnv abstr dicts
  let aligns = readAlignments als
  let ruless = map (tree2rules env) aligns
  writeFile "out/Extracted.tmp" $ prBuiltGrammar env ruless


getGrammarEnv :: FilePath -> [FilePath] -> IO GrammarEnv
getGrammarEnv abstr dicts = do
  syntpgf <- readPGF abstr
  let (_,synt,la,_) = partsOfFileName abstr
  dictpgfs <- mapM readPGF dicts
  return $ GrammarEnv {
    absname = mkCId "Extracted",  --- hard-coded name of generated module
    syntaxpgf = syntpgf,
    absbasemodules = [synt ++ la], --- extending the syntax module
    langenvs = M.fromList [
      (lang,
       LangEnv {
         cncname = mkCId ("Extracted" ++ lang),
         dictpgf = pgf,
         basemodules = [synt++la++lang], --- extending the syntax module
         resourcemodules = [morphodict ++ lang, "Paradigms" ++ lang]
         }) |
              (dict,pgf) <- zip dicts dictpgfs,
              let (_,morphodict,lang,_) = partsOfFileName dict
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
  linrules :: [(LangName,(String,String))],   -- term with its cat
  unknowns :: [(LangName,[(String,String)])]  -- unknown lex item with its cat
  }
  deriving Show

tree2rules :: GrammarEnv -> [(LangName,Tree)] -> BuiltRules
tree2rules env lts = BuiltRules {
  funname = fun,
  linrules = [(lang, (linrule lang tree, showCId cat)) | (lang,tree) <- lts, (cat,_) <- [valcat lang (rootfun tree)]],
  unknowns = [(lang, unknown lang tree) | (lang,tree) <- lts]
  }
 where
   fun = showCId
     (mkFun (concatMap (init . partsOfFun) (concatMap lexitems (map snd lts)))
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
  unwords ["fun",funname br,":",cat,";","--", unwords cats,"--","Abstr"]
  ] ++ [
  mark c (unwords ["lin",funname br,"=",lin,";","--",lang]) | (lang,(lin,c)) <- linrules br
  ] ++ [
  unwords ["oper",fun,"=","mk"++cat, word fun,";","--",lang] | (lang,funcats) <- unknowns br, (fun,cat) <- funcats
  ]
 where
   word f = "\"" ++ takeWhile (/='_') f ++ "\""
   cat:cats = nub (map (snd . snd) (linrules br))
   mark c s = if c==cat then s else "--- " ++ s

prBuiltGrammar env ruless = unlines $ [
   unwords ["abstract", absn, "=",
            concat (intersperse "," (absbasemodules env)), "**","{","-- Abstr"] 
   ] ++ [
   unwords ["concrete", showCId (cncname lenv), "of", absn, "=",
            concat (intersperse ", " (basemodules lenv)), "**",
            "open", concat (intersperse ", " (resourcemodules lenv)), "in","{","--", lang]
     | (lang,lenv) <- M.assocs (langenvs env) 
   ] ++
   map prBuiltRules ruless ++ [
  "} -- " ++ lang | lang <- "Abstr" : langs
   ]
 where
   absn = showCId (absname env)
   langs = M.keys (langenvs env)


-- format:
-- Eng: AdjCN (PositA several_A) (UseN analyst_N)
-- Ita: DetCN diverso_Det (UseN analista_N)
-- 
-- (empty lines between stanzas)

readAlignments :: String -> [[(LangName,Tree)]]
readAlignments = map (map getOne) . stanzas . lines
 where
   getOne line = case words line of  -- Eng: DetCN ...
     lang:tree -> (init lang, getTree (unwords tree))
   getTree s = case readExpr s of
     Just t -> t
     _ -> error ("no tree from: " ++ s)