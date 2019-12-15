module UDOptions where

-- what intermediate phases to show
type Opts = [(String,String)] -- option name, explanation

ifOpt opts o result = case lookup o opts of
  Just msg | isOpt opts "msg" -> putStrMsg (o ++ ", " ++ msg ++ ":") result
  Just msg                    -> putStrLn result
  _ -> return ()

isOpt opts o = elem o (map fst opts)

noOpts = []

fullOpts = [
  ("msg","show message and not just the result"),
  ("ud", "original UD tree in CoNLLU format"),
  ("err","validation errors in the UD tree"),
  ("ut", "tree-structured UD tree"),
  ("dt0","internal development tree, as initialized"),
  ("dt1","internal development tree, with words annotated"),
  ("dt", "final development tree, with all alternatives but no backups"),
  ("bt0","the best (most complete) tree, without backups"),
  ("bt", "the best tree, backups added"),
  ("at0","resulting GF tree, with macros in place"),
  ("at", "final GF tree, macros expanded"),
  ("stat", "show statistics of original and interpreted words")
  ]

minimalOpts = selectOpts ["at"]

defaultOpts = selectOpts ["msg","ud","err","bt0","at","stat"]

selectOpts opts = [(o,s) | (o,s) <- fullOpts, elem o opts]

putStrMsg m s = putStrLn ("# " ++ m) >> putStrLn s
