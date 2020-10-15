module AutoAnnotations where

import GFConcepts
import UDConcepts
import UDAnnotations

import PGF
---- import Data.Graph.Inductive.Query.SP ---
import qualified Data.Map.Strict as M
import Data.List
import Data.Ord
import qualified Data.Set as S

autest = do
  pgf <- readPGF "grammars/Structures.pgf"
  let fls = [unwords ([showCId f, "\t"] ++ functionTypeLabels rglCatHierarchy t ++ ["\t--",prAbsType t])
               | (f,t) <- pgf2functions pgf, length (snd t) > 1]
  putStrLn $ unlines fls

functionTypeLabels :: [Cat] -> AbsType -> [Label]
functionTypeLabels hier (val,args) = case args of
  []  -> []
  [_] -> [head_Label]
  _ -> labels False args
 where
  headArg = minimumBy (comparing (placeInList hier)) args
  labels headFound cs = case cs of
    c:cc | not headFound && c == headArg -> head_Label : labels True cc  -- just one head
    c:cc -> catLabel c : labels headFound cc
    _ -> []

catLabel :: Cat -> Label
catLabel c = "d"++showCId c

-- manually defined hierarchy to decide on heads
---- to be read from a config file
rglCatHierarchy :: [Cat]
rglCatHierarchy = map mkCId $ words
  "VV V2V VP V2 V3 VA VS VQ V2A V2S V2Q NP IP RP CN AP Det IDet Quant Num S RS Adv Conj Prep"

placeInList :: Eq a => [a] -> a -> Int
placeInList xs x = case lookup x (zip xs [0..]) of
  Just n -> n
  _ -> length xs

beforeInList :: Eq a => [a] -> a -> a -> Bool
beforeInList xs x y = case xs of
  e:_ | e == x -> True
  e:_ | e == y -> False
  _:es -> beforeInList es x y
  _ -> False


------------------------------------------------------
--- dependency-graph-based hierarchy does not work, since dependencies are circular

pgf2graph :: PGF -> Graph CId
pgf2graph pgf = [(val,arg) |
  (_,(val,args)) <- pgf2functions pgf,
  arg <- args
  ]

type Graph a = [(a,a)]

edgeMap :: (Ord a) => Graph a -> M.Map a [a]
edgeMap g = M.fromListWith union [(x,[y]) | (x,y) <- g]

paths :: Ord b => Graph b -> b -> b -> [[(b, b)]]
paths g = phs mx
 where
  phs n x y = case M.lookup x mg of
    Just ys | elem y ys -> [[(x,y)]]
    Just ys | n < 1 -> []
    Just ys -> [(x,z):p | z <- ys, z /= x, p <- phs (n-1) z y]
    _ -> []

  mg = edgeMap g
  mx = M.size mg

---- approximating distance
distance :: Ord a => Graph a -> a -> a -> Int
distance g a b = minimum $ map length $ paths g a b


