module RuleBased where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char

-- chart parsing from Peter Ljunglöf, "Pure Functional Parsing", 2002

-- p. 15
type Symb = String
type Token = String

data Rule = Rule {
  lhs :: Symb,
  rhs :: [Symb],
  labels :: [Symb],
  constr :: Symb,
  probab :: Double 
  }
 deriving Show

data Grammar = Grammar {
  rules        :: [Rule],
  terminalmap  :: M.Map Token [Symb],
  catmap       :: M.Map Symb Symb  -- cat to pos
  }
  deriving Show

terminal g t = maybe [t] id $ M.lookup t (terminalmap g)

emptyGrammar = Grammar [] M.empty M.empty

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

parse :: Grammar -> Symb -> [Token] -> [ParseTree]
parse grammar cat input = maybe [] id $
  lookup (0, length input, cat) $
    buildTrees grammar input (passiveEdges (buildChart grammar input))

treeProbability :: Grammar -> ParseTree -> Double
treeProbability grammar = tprob
  where
    tprob t = case t of
      PT f ts -> product (look f : map tprob ts)
      _ -> 1
    look f = maybe 1 id (M.lookup f probmap)
    probmap = M.fromList [(constr r, probab r) | r <- rules grammar]

-------------------
-- textual format
-------------------

pGrammar :: String -> Grammar
pGrammar = combine . addRules . map words . filter relevant . lines
  where
    relevant l = case l of
      '-':'-':_ -> False
      _ | all isSpace l -> False
      _ -> True

    combine (rs,ts,cs) = Grammar rs (M.fromListWith (++) ts) (M.fromList cs)

    addRules = foldr addRule ([],[],[])
    
    addRule ws g@(rs,ts,cs) = case ws of
      "#token":c:ww -> (rs, [(w,[c]) | w <- ww] ++ ts, cs)

      "#cat":c:ww   -> (rs, ts,[(w,c) | w <- ww] ++ cs)
      f:c:"::=":ww | last f == '.' -> (
        getRule (unwords ws) (init f) c (splitSemic ww) : rs, ts,cs)
      _ -> error ("rule not parsed: " ++ unwords ws)

    getRule s f c wws = case wws of
      [cs,labs,[p]] -> prule f c cs labs (read p)
      [cs,labs] -> drule f c cs labs
      [cs] -> erule f c cs
      _ -> error ("ill-formed rule: " ++ s)

    splitSemic ws = case break (==";") ws of
      (cs,_:rest) -> cs : splitSemic rest
      ([],_) -> [] 
      (cs,_) -> [cs]
 

erule :: Symb -> Symb -> [Symb] -> Rule
erule f c cs = Rule c cs ["head"] f 1

drule :: Symb -> Symb -> [Symb] -> [Symb] -> Rule
drule f c cs ds = Rule c cs ds f 1

prule :: Symb -> Symb -> [Symb] -> [Symb] -> Double -> Rule
prule f c cs ds p = Rule c cs ds f p

