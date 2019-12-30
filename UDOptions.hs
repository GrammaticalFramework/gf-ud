module UDOptions where

-- what intermediate phases to show

-- ud2gf
minimalOptsUD2GF = selectOpts ["at"]
defaultOptsUD2GF = selectOpts ["msg","ud","err","bt0","at","tc","stat"]

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
  ("bt", "the best tree, backups added"),
  ("at0","resulting GF tree, with macros in place"),
  ("at", "final GF tree, macros expanded"),
  ("tc", "type checking the final GF tree"),
  ("gf", "(gf2ud) original GF tree"),
  ("an0","(gf2ud) initial annotated tree"),
  ("an1","(gf2ud) annotated tree with labels"),
  ("an2","(gf2ud) annotated tree with labels and words"),
  ("an3","(gf2ud) final annotated tree with nonlocal operations"),
  ("vat", "visualize abstract syntax tree using Graphviz"),
  ("vud", "visualize dependency tree using LaTeX"),
  ("stat", "show statistics of original and interpreted words")
  ]

