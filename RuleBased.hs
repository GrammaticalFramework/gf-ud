module RuleBased where

import qualified Data.Set as S
import qualified Data.Map as M

-- chart parsing from Peter Ljunglöf, "Pure Functional Parsing", 2002

-- p. 15
type Symb = String
type Token = String

data Rule = Rule {
  lhs :: Symb,
  rhs :: [Symb],
  labels :: [Symb],
  constr :: Symb
  }
 deriving Show

data Grammar = Grammar {
  rules     :: [Rule],
  startcat  :: Symb,
  terminal  :: Token -> [Symb]
  }


-- p. 64
type Edge  = (Int,Symb,[Symb])
type State = S.Set Edge
type Chart = [State]

buildChart :: Grammar -> [Token] -> Chart
buildChart grammar input = finalChart
  where
  
    finalChart :: Chart
    finalChart = map buildState initialChart

    initialChart :: Chart
    initialChart = S.empty : map initialState (zip [0..] input)

    initialState :: (Int,Token) -> State
    initialState (i,tok) = S.fromList [(i,cat,[]) | cat <- terminal grammar tok]

    buildState :: State -> State
    buildState = limit more

    more :: Edge -> State
    more e@(j,a,cats) = case cats of
      [] -> S.fromList [(j, lhs r, bs) |
                 r <- rules grammar,
                 a':bs <- [rhs r],
                 a == a'
                 ]
          `S.union`
            S.fromList [(i, b, bs) |
                 (i, b, a':bs) <- S.toList (finalChart !! j),
                 a == a'
                 ]
      _ -> S.empty

-- p. 14
limit :: Ord a => (a -> S.Set a) -> S.Set a -> S.Set a
limit more start = limit' start start
   where
     limit' old new
       | S.null new' = old
       | otherwise    = limit' (S.union new' old) (S.difference new' old)
      where
        new' = S.unions (S.toList (S.map more new))


-- p. 66

type Passive = (Int,Int,Symb)

passiveEdges :: Chart -> [Passive]
passiveEdges chart = [
  (i, j, cat) |
    (j,state) <- zip [0..] chart,
    (i,cat,[]) <- S.toList state
    ]

data ParseTree =
    PT Symb [ParseTree]
  | PL Token
 deriving Show

buildTrees :: Grammar -> [Token] -> [Passive] -> [(Passive,[ParseTree])]
buildTrees grammar input passives = edgeTrees

  where
  
    edgeTrees :: [(Passive,[ParseTree])]
    edgeTrees = [(pe, treesFor pe) | pe <- passives]

    treesFor (i,j,cat) = [
      PT (constr rule) trees |
        rule <- rules grammar,
        lhs rule == cat,  ---- TODO: rule <- rules grammar cat
        trees <- children (rhs rule) i j
      ] ++ [
      PT cat [PL sym] |
        i == j-1,
        let sym = input !! i,
        elem cat (terminal grammar sym)
      ]

    children :: [Symb] -> Int -> Int -> [[ParseTree]]
    children cs i k = case cs of
      []   -> [[] | i == k]
      c:cs -> [
        tree : rest |
          i <= k,
          ((i',j,c'),trees) <- edgeTrees,
          i == i', c == c',
          rest <- children cs j k,
          tree <- trees
        ]

parse :: Grammar -> [Token] -> [ParseTree]
parse grammar input = maybe [] id $
  lookup (0, length input, startcat grammar) $
    buildTrees grammar input (passiveEdges (buildChart grammar input))


--- to test

erule :: Symb -> Symb -> [Symb] -> Rule
erule f c cs = Rule c cs ["head"] f

drule :: Symb -> Symb -> [Symb] -> [Symb] -> Rule
drule f c cs ds = Rule c cs ds f

kg :: Grammar
kg = Grammar {
  rules = [
    drule "Pred"  "S"   ["NP","VP"] ["nsubj","head"],
    drule "AdvVP" "VP"  ["VP","Adv"] ["head","advmod"],
    erule "UseV"  "VP"  ["V"],
    drule "Compl" "VP"  ["V","NP"] ["head","obj"],
    drule "UseAdv" "VP" ["Cop","Adv"] ["cop","head"],
    erule "UsePN" "NP"  ["PN"],
    drule "AdvNP" "NP"  ["NP","Adv"] ["head","advmod"]
    ],
  startcat = "S",
  terminal = \t -> case t of
    "hello" -> ["S"]
    "John"  -> ["PN"]
    "walks" -> ["V"]
    "well"  -> ["Adv"]
    "is"    -> ["Cop"]
    _ -> []
  }

