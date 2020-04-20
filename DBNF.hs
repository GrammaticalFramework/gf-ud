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
    grfile:startcat:opts -> processRuleBased grfile startcat opts
    _ -> putStrLn usage

processRuleBased grfile startcat opts = do
  gr <- readFile grfile >>= return . pGrammar
  -- putStrLn $ unlines $ map show (rules gr) ---- debug
  putStrLn $ checkGrammar gr
  interact (unlines . map (processOne gr startcat opts) . filter (not . all isSpace) . lines)
      
usage = "usage: RuleBased <grammar> <startcat> (-parsetrees | -deptrees) (-<number>)"

processOne :: Grammar -> Symb -> [String] -> String -> String
processOne gr cat opts s = case opts of
  _ | elem "-onlyparsetrees" opts -> unlines ["# parsetree = " ++ pt | pt <- ptrees]
  _ -> unlines $ [
    "# text = " ++ s,
    "# analyses = " ++ show (length rts)
    ] ++ dtreess
  where
    dtreess = map unlines [ 
      ["# parsetree = " ++ pt, dt] | (pt,dt) <- zip ptrees dtrees
      ]
    doshow = case filter (isPrefixOf "-show") opts of
      cut:_ -> take (read (drop 6 cut))
      _ -> take 1
    docut  = case filter (isPrefixOf "-cut") opts of
      cut:_ -> take (read (drop 5 cut))
      _ -> id
    parses = doshow raws
    (totals,chunks) = parse gr cat (words s)
    raws   = if null rts then chunks else rts
    rts    = rankTrees $ docut totals 
    ptrees = map prParseTree parses
    dtrees = map (prDepTree . markDependencies gr) parses

-- chart parsing from Peter Ljunglöf, "Pure Functional Parsing", 2002

-- p. 15
type Symb = String
type Token = String

data Rule = Rule {
  constr :: Symb,
  lhs :: Symb,
  rhs :: [Symb],
  labels :: [Symb],
  weight :: Double 
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
  let
    ts =
       [c |                           Just cs <- [M.lookup t (terminalmap g)], c <- cs] ++
       [c | (s, Just p) <- [unPOS t], Just cs <- [M.lookup s (terminalmap g)], c <- cs] ++
       [c | (s, Just p) <- [unPOS t], Just cs <- [M.lookup p (posmap g)],      c <- cs]
  in if (null ts) then ["Str"] else ts

-- instead of having a word in the lexicon, mark it in input as word:<POS> where POS matches a category
--- a bit complicated because of 11:30:<NUM>
unPOS :: Token -> (Token,Maybe Symb)
unPOS t = case break (==':') (reverse t) of
  (p@(_:_),_:s) | head p == '>' && last p == '<' -> (reverse s, Just (reverse (tail (init p))))
  _ -> (t,Nothing)

-- lower weight to ad-hoc words
tokenWeight :: Token -> Double
tokenWeight t = maybe 0.5 (const 0.2) $ snd $ unPOS t

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
    PT (Symb,Symb,[Symb],Double) [ParseTree]   -- (cat,constr,labels,weight) subtrees
  | PL (Symb,Token) (Int,Symb,Int,Double) -- (cat,terminal) (position, label, head, weight)
 deriving Show

buildTrees :: Grammar -> [Token] -> [Passive] -> [(Passive,[ParseTree])]
buildTrees grammar input passives = edgeTrees

  where
  
    edgeTrees :: [(Passive,[ParseTree])]
    edgeTrees = [(pe, treesFor pe) | pe <- passives]

    treesFor (i,j,cat) = [
      PT (cat, constr rule, labels rule, weight rule) trees |
        rule <- rules grammar,
        lhs rule == cat,  ---- TODO: rule <- rules grammar cat
        trees <- children (rhs rule) i j
      ] ++ [
      PL (cat,fst (unPOS tok)) (j,"dep",0, tokenWeight tok) | -- default label and head
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

parse :: Grammar -> Symb -> [Token] -> ([ParseTree],[ParseTree])
parse grammar cat input = (completes,chunks)
  where
    completes = maybe [] id $ lookup (0, length input, cat) $ subtrees
    chunks    = [chunkParse subtrees]
    subtrees  = buildTrees grammar input (passiveEdges (buildChart grammar input))

chunkParse :: [(Passive,[ParseTree])] -> ParseTree
chunkParse subtrees = 
    PT chunknode subs 
  where
    subs = next 0 subtreelist
    chunknode = ("Chunks", "chunks", replicate (length subs - 1) "dep" ++ ["head"], 0.0000001)

    next :: Int -> [((Int,Int),ParseTree)] -> [ParseTree]
    next i sl = case sl of
      [] -> []
      ((k,j),t):ss | k == i -> t : next j ss
      _:ss -> next i ss

    subtreelist :: [((Int,Int),ParseTree)]
    subtreelist = sortOn (\ ((i,j),_) -> (i,0-j))
      [((i,j),t) |
        ((i,j,_),ts) <- subtrees, t:_ <- [ts] --- [rankTrees ts]
        ]

-- context-free weight ("probability") of a tree

treeWeight :: ParseTree -> Double
treeWeight = tprob
  where
    tprob t = case t of
      PT (_,f,_,w) ts -> product (w : map tprob ts)
      PL _ (_,_,_,w) -> w

-- sort by descending weight
rankTrees :: [ParseTree] -> [ParseTree]
rankTrees = sortOn ((1-) . treeWeight)

-- mark dependency labels and heads in leaf nodes
-- simplified version of Kolachina and Ranta, LiLT 2016

markDependencies :: Grammar -> ParseTree -> ParseTree
markDependencies grammar =
    mark ("root",0) .
    annotate
  where
    annotate pt = case pt of
      PT (cat,fun,ds,w) pts -> PT (cat,fun, ds, w) (map annotate pts)
      PL (cat,tok) info -> PL (lookc cat,tok) info
      
    mark (lab,hd) pt = case pt of
      PL tok (i,_,_,w) -> PL tok (i,lab,hd,w)
      PT (cat,fun,labs,w) pts ->
        PT (cat,fun,labs,w) [
          markIf (lab,hd) (l,h) l t
            | let tls = zip pts labs,
              let h = headTok tls,
              (t,l) <- tls
              ]
    markIf labhd lh l t = case l of
      "head" -> mark labhd t
      _ -> mark lh t
    headTok tls = case filter ((=="head") . snd) tls of
      (PL _ (i,_,_,_),_):_ -> i
      (PT (_,_,ls,_) ts,_):_ -> headTok (zip ts ls)

    lookc cat = maybe cat id (M.lookup cat (catmap grammar))

------------------------------
-- printing trees
------------------------------

prParseTree :: ParseTree -> String
prParseTree pt = case pt of
  PT (cat,fun,_,_) pts -> parenth (unwords (trim cat : map prParseTree pts))
  PL (cat,tok) _ -> parenth (unwords [trim cat, trim tok])
 where
   parenth s = "(" ++ s ++ ")"
   trim c = case c of --- for printing the tree via GF, make identifiers valid
     '\'':_ -> c ++ "'"
     x:xs | not (isLetter x && all (\y -> isAlphaNum y || elem y "_'") xs) -> "'" ++ c ++ "'"
     _ -> c

prDepTree :: ParseTree -> String
prDepTree = unlines . map prOne . getTokens
  where
    getTokens pt = case pt of
      PT _ pts -> concatMap getTokens pts
      PL (pos,tok) (i,lab,hd,_) -> [(show i,tok,pos,show hd,lab)]
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

    combine (rs,ts,cs) = Grammar (numRules rs) (M.fromListWith (++) ts) (M.fromList (("Str","X") : cs)) (posm cs)

    addRules = foldr addRule ([],[],[])
    
    addRule ws g@(rs,ts,cs) = case ws of
      "#token":c:ww -> (rs, [(w,[c]) | w <- ww] ++ ts, cs)

      "#pos":c:ww   -> (rs, ts,[(w,c) | w <- ww] ++ cs)
      c:"::=":ww -> (
        expandRule (getRule (unwords ws) c (splitSemic ww)) ++ rs, ts,cs)
      _ -> error ("rule not parsed: " ++ unwords ws)

    expandRule r = [
      Rule (constr r) (lhs r) cs labs (weight r) |
        (cs,labs) <- combinations (zip (map optionalize (rhs r)) (labels r))
      ]

   -- optional category with ?, e.g. neg? -> Left neg
    optionalize c = case c of
      _ | last c == '?' -> Left (init c)
      _ -> Right c

    combinations :: [(Either Symb Symb,Symb)] -> [([Symb],[Symb])]
    combinations cls = [
        unzip (concat cl) |
          let combs (c,l) = case c of
                Left  c -> [[(c,l)], []] 
                Right c -> [[(c,l)]],
          let mcls = map combs cls,
          cl  <- sequence mcls
        ]

    getRule s c wws = case wws of
      [cs,labs,[p]] -> Rule "" c cs (fixLabs cs labs) (read p)
      [cs,labs] -> Rule "" c cs (fixLabs cs labs) 0.5
      [cs] -> Rule "" c cs (fixLabs cs []) 0.5
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
