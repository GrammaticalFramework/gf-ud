module UDVisualization where

import UDConcepts
import GFConcepts
import UDAnnotations

import DBNF as D

import PGF

import System.Process (system)


visualizeAbsTrees :: UDEnv -> [AbsTree] -> IO ()
visualizeAbsTrees env ts = do
  let astfile = "_gfud_ast_tmp"
  absTrees2latex env astfile ts
  system $ "pdflatex " ++ (astfile ++ ".tex") ++ " >/dev/null"
  system $ "open " ++ (astfile ++ ".pdf") ---- TODO: parameterize open command
  return () 

absTrees2latex :: UDEnv -> FilePath -> [AbsTree] -> IO ()
absTrees2latex env file ts = do
  let exps = map abstree2expr ts
  let codes = map (graphvizAbstractTree (pgfGrammar env) (True,False)) exps
  let astDotFile i suff = "_" ++ show i ++ file ++ "." ++ suff
  mapM_ (\ (i,code) -> writeFile (astDotFile i "dot")  code) (zip [1..] codes)
  mapM_ (\i -> system ("dot -Teps " ++ astDotFile i "dot" ++ " >" ++ astDotFile i "eps")) [1.. length ts]
  writeFile (file ++ ".tex") $ unlines [
    "\\documentclass{article}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{graphicx}",
    "\\begin{document}",
    ""
    ]
  let treeSize t = case length (leavesRTree t) of
        n | n < 3 -> 0.2
        n | n < 5 -> 0.4
        n | n < 8 -> 0.6
        n | n < 12 -> 0.8
        _ -> 1.0
  let width i = show (treeSize (ts !! i))
  mapM_ (\i -> appendFile (file ++ ".tex") ("\n\n\\includegraphics[width=" ++ width (i-1) ++ "\\textwidth]{" ++ astDotFile i "eps" ++ "}")) [1.. length ts]
  appendFile (file ++ ".tex") "\\end{document}"

visualizeUDSentences :: [UDSentence] -> IO ()
visualizeUDSentences uds = do
  let doc = ud2latex uds
  writeFile "_ud_tmp.tex" doc
  system $ "pdflatex _ud_tmp.tex >/dev/null"
  system $ "open _ud_tmp.pdf" ---- TODO: parameterize open command
  return () 

ud2latex :: [UDSentence] -> String
ud2latex = 
  conlls2latexDoc .
  map (unlines  . map (trim . prt) . udWordLines)
 where
   trim = concatMap (\c -> if elem c "%$&" then "\\"++[c] else [c])

--- pretends that parse trees are abstract trees, for easy visualization
visualizeParseTrees :: [D.ParseTree] -> IO ()
visualizeParseTrees = visualizeAbsTrees initUDEnv . map p2a
  where
   p2a pt = case pt of
     PT (cat,_,_,_) pts -> RTree (mkCId cat) (map p2a pts)
     PL (cat,tok) _     -> RTree (mkCId cat) [RTree (mkCId tok) []]

-- real parse tree
-- graphvizBracketedString :: GraphvizOptions -> Maybe Labels -> Tree -> [BracketedString] -> String
--   graphvizDefaults :: GraphvizOptions
--   type Labels = Map CId [String]
--   Tree

selectParseTrees :: [String] -> [String]
selectParseTrees ls = [unwords ws | l <- ls, "#":"parsetree": "=" : ws <- [words l]]

