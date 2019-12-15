module UDVisualization where

import UDConcepts
import GFConcepts
import UDAnnotations

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

visualizeUDSentences :: UDEnv -> [UDSentence] -> IO ()
visualizeUDSentences env uds = do
  let ss = map (unlines  . map prt . udWordLines) uds
  let doc = conlls2latexDoc ss
  writeFile "_ud_tmp.tex" doc
  system $ "pdflatex _ud_tmp.tex"
  system $ "open _ud_tmp.pdf" ---- TODO: parameterize open command
  return () 


