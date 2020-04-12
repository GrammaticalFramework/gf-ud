module DBNF where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Char
import Data.List
import System.Environment (getArgs)

-- main: read DBNF grammar (dependency BNF), parse stdio line by line, print
-- bracketed parse trees and CoNLL dependency trees
-- example: echo "the cat is old" | runghc RuleBased.hs grammars/English.dbnf Text

main = do
  xx <- getArgs
  case xx of
    grfile:startcat:[] -> processRuleBased grfile startcat
    _ -> putStrLn usage

processRuleBased grfile startcat = do
  gr <- readFile grfile >>= return . pGrammar
  putStrLn $ checkGrammar gr
  interact (unlines . map (processOne gr startcat) . lines)
      
usage = "usage: RuleBased <grammar> <startcat>"

processOne gr cat s = unlines $ [
  "# sentence: " ++ s,
  "# ambiguity: " ++ show (length parses),
  "# parse tree: " ++ ptree,
  dtree
  ]
 where
   parses = rankTrees gr (parse gr cat (words s))
   ptree = case parses of
     t:_ -> prParseTree t
     _ -> "NONE"
   dtree = case parses of
     t:_ -> prDepTree $ markDependencies gr t
     _ -> ""

-- chart parsing from Peter Ljunglöf, "Pure Functional Parsing", 2002

-- p. 15
type Symb = String
type Token = String

data Rule = Rule {
  constr :: Symb,
  lhs :: Symb,
  rhs :: [Symb],
  labels :: [Symb],
  probab :: Double 
  }
 deriving Show

data Grammar = Grammar {
  rules        :: [Rule],
  terminalmap  :: M.Map Token [Symb], -- token to cats
  catmap       :: M.Map Symb Symb,    -- cat to UD pos
  posmap       :: M.Map Symb [Symb]   -- pos to cats
  }
  deriving Show

terminal :: Grammar -> Token -> [Symb]
terminal g t =
  [c | Just cs <- [M.lookup t (terminalmap g)], c <- cs] ++
  [c | (s, Just p) <- [unPOS t], Just cs <- [M.lookup p (posmap g)], c <- cs]

-- instead of having a word in the lexicon, mark it in input as word:<POS> where POS matches a category
--- a bit complicated because of 11:30:<NUM>
unPOS :: Token -> (Token,Maybe Symb)
unPOS t = case break (==':') (reverse t) of
  (p@(_:_),_:s) | head p == '>' && last p == '<' -> (reverse s, Just (reverse (tail (init p))))
  _ -> (t,Nothing)


emptyGrammar = Grammar [] M.empty M.empty M.empty

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
    PT (Symb,Symb,[Symb]) [ParseTree]   -- (cat,constr,labels) subtrees
  | PL (Symb,Token) (Int,Symb,Int) -- (cat,terminal) (position, label, head)
 deriving Show

buildTrees :: Grammar -> [Token] -> [Passive] -> [(Passive,[ParseTree])]
buildTrees grammar input passives = edgeTrees

  where
  
    edgeTrees :: [(Passive,[ParseTree])]
    edgeTrees = [(pe, treesFor pe) | pe <- passives]

    treesFor (i,j,cat) = [
      PT (cat, constr rule,[]) trees |
        rule <- rules grammar,
        lhs rule == cat,  ---- TODO: rule <- rules grammar cat
        trees <- children (rhs rule) i j
      ] ++ [
      PL (cat,fst (unPOS tok)) (j,"dep",0) | -- default label and head
        i == j-1,
        let tok = input !! i,
        elem cat (terminal grammar tok)
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

-- context-free probability of a tree

treeProbability :: Grammar -> ParseTree -> Double
treeProbability grammar = tprob
  where
    tprob t = case t of
      PT (_,f,_) ts -> product (look f : map tprob ts)
      _ -> 1
    look f = maybe 1 id (M.lookup f probmap)
    probmap = M.fromList [(constr r, probab r) | r <- rules grammar]

-- sort by descending probability
rankTrees :: Grammar -> [ParseTree] -> [ParseTree]
rankTrees gr = sortOn ((1-) . treeProbability gr)

-- mark dependency labels and heads in leaf nodes
-- simplified version of Kolachina and Ranta, LiLT 2016

markDependencies :: Grammar -> ParseTree -> ParseTree
markDependencies grammar =
    mark ("root",0) .
    annotate
  where
    annotate pt = case pt of
      PT (cat,fun,_) pts -> PT (cat,fun,lookf fun) (map annotate pts)
      PL (cat,tok) info -> PL (lookc cat,tok) info
      
    mark (lab,hd) pt = case pt of
      PL tok (i,_,_) -> PL tok (i,lab,hd)
      PT (cat,fun,labs) pts ->
        PT (cat,fun,labs) [
          markIf (lab,hd) (l,h) l t
            | let tls = zip pts labs,
              let h = headTok tls,
              (t,l) <- tls
              ]
    markIf labhd lh l t = case l of
      "head" -> mark labhd t
      _ -> mark lh t
    headTok tls = case filter ((=="head") . snd) tls of
      (PL _ (i,_,_),_):_ -> i
      (PT (_,_,ls) ts,_):_ -> headTok (zip ts ls)

    lookf fun = maybe ("head" : repeat "dep") id (M.lookup fun labelMap)
    labelMap = M.fromList [(constr r, labels r) | r <- rules grammar]

    lookc cat = maybe cat id (M.lookup cat (catmap grammar))

------------------------------
-- printing trees
------------------------------

prParseTree :: ParseTree -> String
prParseTree pt = case pt of
  PT (cat,fun,_) pts -> parenth (unwords (cat : map prParseTree pts))
  PL (cat,tok) _ -> parenth (unwords [cat,tok])
 where
   parenth s = "(" ++ s ++ ")"

prDepTree :: ParseTree -> String
prDepTree = unlines . map prOne . getTokens
  where
    getTokens pt = case pt of
      PT _ pts -> concatMap getTokens pts
      PL (pos,tok) (i,lab,hd) -> [(show i,tok,pos,show hd,lab)]
    prOne (i,t,p,h,d) = concat (intersperse "\t" [i,t,unc,p,unc,unc,h,d,unc,unc])
    unc = "_"
 
------------------------------
-- textual format of DBNF grammars
-------------------------------
-- Simple format, one rule per line, comment lines start --.
-- Phrase structure rule format: LHS ::= RHS (# labels (# weight))?
--
--   Cl ::= NP do not VP  # nsubj aux advmod head # 0.7
--
-- All symbols in LHS and RHS are nonterminals.
-- Terminals are introduces by rules of form: #token NONTERM words
--
--   #token do do does did done
--   #token N man men cat cats
--
-- Universal pos tags are defined by rules of form: #pos POS nonterms
--
--   #pos ADV Adv IAdv not
--

pGrammar :: String -> Grammar
pGrammar = combine . addRules . map words . filter relevant . lines
  where
    relevant l = case l of
      '-':'-':_ -> False
      _ | all isSpace l -> False
      _ -> True

    combine (rs,ts,cs) = Grammar (numRules rs) (M.fromListWith (++) ts) (M.fromList cs) (posm cs)

    addRules = foldr addRule ([],[],[])
    
    addRule ws g@(rs,ts,cs) = case ws of
      "#token":c:ww -> (rs, [(w,[c]) | w <- ww] ++ ts, cs)

      "#pos":c:ww   -> (rs, ts,[(w,c) | w <- ww] ++ cs)
      c:"::=":ww -> (
        getRule (unwords ws) c (splitSemic ww) : rs, ts,cs)
      _ -> error ("rule not parsed: " ++ unwords ws)

    getRule s c wws = case wws of
      [cs,labs,[p]] -> Rule "" c cs (fixLabs cs labs) (read p)
      [cs,labs] -> Rule "" c cs (fixLabs cs labs) 1
      [cs] -> Rule "" c cs (fixLabs cs []) 1
      _ -> error ("ill-formed rule: " ++ s)

    fixLabs cs labs = if length cs == 1 then ["head"] else labs
    
    numRules rs = [Rule ("R" ++ show i) c cs labs p |
                    (i,Rule _ c cs labs p) <- zip [1..] rs]

    posm cs = M.fromListWith (++) [(p,[c]) | (c,p) <- cs]

    splitSemic ws = case break (flip elem [";","#"]) ws of
      (cs,_:rest) -> cs : splitSemic rest
      ([],_) -> [] 
      (cs,_) -> [cs]

checkGrammar :: Grammar -> String
checkGrammar g = case checks g of
  [] -> []
  cs -> error $ unlines $ "Errors in grammar:" : cs
 where
   checks g =
     ["invalid labels in " ++ show r | r <- rules g, length (rhs r) /= length (labels r) || noUniqueHead r]
   noUniqueHead r = length (lhs r) > 0 && length (filter (=="head") (labels r)) /= 1
