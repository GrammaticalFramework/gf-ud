module UDVisualization where

import UDConcepts
import GFConcepts
import UDAnnotations

import DBNF as D

import PGF

import System.Process (system)


visualizeAbsTrees :: UDEnv -> [AbsTree] -> IO ()
visualizeAbsTrees env ts = do
  let exps = map abstree2expr ts
  let codes = map (graphvizAbstractTree (pgfGrammar env) (True,False)) exps
  let astFile i suff = "_" ++ show i ++ "_ud_ast_tmp." ++ suff
  mapM_ (\ (i,code) -> writeFile (astFile i "dot")  code) (zip [1..] codes)
  mapM_ (\i -> system ("dot -Teps " ++ astFile i "dot" ++ " >" ++ astFile i "eps")) [1.. length ts]
  writeFile (astFile 0 "tex") $ unlines [
    "\\documentclass{article}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage{graphicx}",
    "\\begin{document}",
    ""
    ]
  mapM_ (\i -> appendFile (astFile 0 "tex") ("\n\n\\includegraphics[width=0.8\\textwidth]{" ++ astFile i "eps" ++ "}")) [1.. length ts]
  appendFile (astFile 0 "tex") "\\end{document}"
  system $ "pdflatex " ++ (astFile 0 "tex")
  system $ "open " ++ (astFile 0 "pdf") ---- TODO: parameterize open command
  return () 

visualizeUDSentences :: [UDSentence] -> IO ()
visualizeUDSentences uds = do
  let doc = ud2latex uds
  writeFile "_ud_tmp.tex" doc
  system $ "pdflatex _ud_tmp.tex"
  system $ "open _ud_tmp.pdf" ---- TODO: parameterize open command
  return () 

ud2latex :: [UDSentence] -> String
ud2latex = 
  conlls2latexDoc .
  map (unlines  . map prt . udWordLines)

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

