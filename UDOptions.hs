module UDOptions where

-- what intermediate phases to show

-- ud2gf
minimalOptsUD2GF = selectOpts ["at"]
defaultOptsUD2GF = selectOpts ["msg","ud","err","bt0", "bt0me","at","tc","lin","sum","stat"]
nonPrintingOpts = selectOpts ["no-backups"]

-- gf2ud
minimalOptsGF2UD = selectOpts ["ud"]
defaultOptsGF2UD = selectOpts ["msg","gf","an3","ut","ud"]

selectOpts opts = [(o,s) | (o,s) <- fullOpts, elem o opts]
putStrMsg m s = putStrLn ("# " ++ m) >> putStrLn s



type Opts = [(String,String)] -- option name, explanation

ifOpt opts o result = case lookup o opts of
  Just msg | isOpt opts "msg" -> putStrMsg (o ++ ", " ++ msg ++ ":") result
  Just msg                    -> putStrLn result
  _ -> return ()

isOpt opts o = elem o (map fst opts)

noOpts = []

fullOpts = [
  ("msg","show message and not just the result"),
  ("ud", "UD tree in CoNLLU format"),
  ("err","validation errors in the UD tree"),
  ("ut", "tree-structured UD tree"),
  ("dt0","internal development tree, as initialized"),
  ("dt1","internal development tree, with words annotated"),
  ("dt", "final development tree, with all alternatives but no backups"),
  ("bt0","the best (most complete) tree, without backups"),
  ("bt0me","the same as bt0, but with macros expanded"),
  ("bt", "the best tree, backups added"),
  ("at0","resulting GF tree, with macros in place"),
  ("at", "final GF tree, macros expanded"),
  ("tc", "type checking the final GF tree"),
  ("no-backups", "don't add backups to incomplete trees"),
  ("sum","summary: GF tree built from the interpreted nodes"),
  ("gf", "(gf2ud) original GF tree"),
  ("an0","(gf2ud) initial annotated tree"),
  ("an1","(gf2ud) annotated tree with labels"),
  ("an2","(gf2ud) annotated tree with labels and words"),
  ("an3","(gf2ud) final annotated tree with nonlocal operations"),
  ("vat", "visualize abstract syntax tree using Graphviz"),
  ("vud", "visualize dependency tree using LaTeX converted to pdf"),
  ("lud", "visualize dependency tree in LaTeX and output the LaTeX code"),
  ("lin", "linearize tree using the active language of the environment"),
  ("units", "(eval) print evaluation per sentence, from lowest score upwards"),
  ("stat", "show statistics of original and interpreted words"),
  ("adjust", "(pattern-match) adjust results to a valid UD trees with root"), 
  ("prune", "(pattern-match) show only the roots of matching subtrees"),
  ("FORM", "(statistics) surface forms"),
  ("LEMMA", "(statistics) lemmas"),
  ("POS", "(statistics) part of speech tags"),
  ("FEATS", "(statistics) morphological features"),
  ("DISTANCE", "(statistics) distance to head i.e. head_position - own_position"),
  ("DEPREL", "(statistics) dependency labels"),
  ("SUBTREETYPE", "(statistics) types of subtrees"),
  ("LENGTH", "(statistics) number of words in a sentence"),
  ("DEPTH", "(statistics) depth of dependency tree")
  ]

