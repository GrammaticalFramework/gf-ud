module Backend where

import UDAnnotations
import PGF

transferTree :: UDEnv -> Expr -> Expr
transferTree env e = e

linearizeTree :: UDEnv -> Language -> Expr -> String
linearizeTree env lang = linearize (pgfGrammar env) lang



