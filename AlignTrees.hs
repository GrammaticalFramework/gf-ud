module AlignTrees where

--import TreeConv
--import GetConfig (getSentences)
import UDConcepts
import GFConcepts

import Data.List
import qualified Data.Map as M
--import System.Environment (getArgs)

-- read conll format from two files, which contain parallel UD trees

testAlignUDFiles :: FilePath -> FilePath -> IO ()
testAlignUDFiles a b = do
  ts <- parseUDFile a >>= return . map udSentence2tree
  us <- parseUDFile b >>= return . map udSentence2tree
  testAlignUDTrees ts us

testAlignUDTrees :: [UDTree] -> [UDTree] -> IO ()
testAlignUDTrees ts us = do
  if length ts /= length us
    then error "cannot align tree lists of different length"
    else return ()
  let tus = zip ts us
  
  -- aligned trees
  let atus  = map (uncurry alignUDTrees) tus
  
  -- subtree alignments found in each tree
  let past (t,u) = udUPOS (root t) ++ "/" ++ udUPOS (root u) ++ " : " 
                   ++ prAlignment 2 (prUDTree t, prUDTree u)
  let pastus = [(prUDTree (fst tu)) ++ (prUDTree (snd tu)) 
                  ++ uncurry prAlignedUDTrees atu 
                  ++ unlines (map past (alignedUDSubtrees atu)) 
                | (tu,atu) <- zip tus atus]
  mapM_ putStrLn pastus

padUDWord   :: UDWord
isPadUDWord :: UDWord -> Bool
padUDWord     = (initUDWord 666){udDEPREL=pad_Label}
isPadUDWord n = udDEPREL n == pad_Label
pad_Label = "pad"


-- align two dep trees by sorting and padding
alignUDTrees :: UDTree -> UDTree -> (UDTree,UDTree)
alignUDTrees = alignRTrees (udDEPREL . root) (\t -> (udDEPREL (root t), dependencyDistance (root t))) (RTree padUDWord [])

-- align two trees by sorting and padding, with padding key k, sorting key h, and default value d 
alignRTrees :: (Ord b,Ord c) => (RTree a -> b) -> (RTree a -> c) -> RTree a -> RTree a -> RTree a -> (RTree a, RTree a)
alignRTrees k h d (RTree n ts) (RTree m us) = (RTree n pts, RTree m pus)
  where
   (pts,pus) = unzip [alignRTrees k h d t u | (_,(t,u)) <- padLists k d (sorts ts) (sorts us)]
   sorts     = sortOn h

-- auxiliary: pad two parallel lists with a default value d; sorting is presupposed 
padLists :: Ord a => (b -> a) -> b -> [b] -> [b] -> [(a,(b,b))]
padLists k d xs ys = pads xs ys
  where
    pads xx yy = case (xx,yy) of
      (x:xx2, y:yy2)
        | k x == k y -> (k x, (x,y)) : pads xx2 yy2
        | k x <  k y -> (k x, (x,d)) : pads xx2 yy
        | k x >  k y -> (k y, (d,y)) : pads xx  yy2
      (_, []) -> [(k x,(x,d)) | x <- xx]
      ([], _) -> [(k y,(d,y)) | y <- yy]
      _ -> []

-- aligned subtrees; assumes perfect alignment; add a tree for each head word, too
alignedSubtrees :: (RTree a,RTree a) -> [(RTree a,RTree a)]
alignedSubtrees (t,u) = zip (subs t) (subs u) 
  where
   subs t = let ts = subtrees t in ts ++ [RTree n [] | RTree n (_:_) <- ts]

-- corollary: aligned trees with pad nodes eliminated
alignedUDSubtrees :: (UDTree,UDTree) -> [(UDTree,UDTree)]
alignedUDSubtrees tu = [(unPad t,unPad u) | (t,u) <- alignedSubtrees tu, notPad t, notPad u]
  where
    notPad t = not (isPadUDWord (root t))
    unPad (RTree n ts) = RTree n (filter notPad ts)

-- print aligned trees line by line, separated by |||
prAlignedUDTrees :: UDTree -> UDTree -> String
prAlignedUDTrees t u = unlines prints 
  where
    pt = lines $ prUDTree t
    pu = lines $ prUDTree u
    mx = maximum $ map length pt
    prints = [prAlignment mx xy | xy <- zip pt pu]

prUDTreeString :: UDTree -> String
prUDTreeString t = unwords [udFORM n | n <- sortOn udID (allNodesRTree t)]

prAlignment :: Int -> (String,String) -> String
prAlignment mx (x,y) = x ++ (replicate (mx - length x) ' ') ++ "  |||  " ++ y


---- TODO: ranking

-- disambiguation many-to-many alignments to result in one-to-one alignment
-- many-to-many alignments can not always be disambiguated. use default value
-- d to disambiguate if needed
-- based on number of non-padded nodes for each tree pair
bestAlignment :: UDTree -> (UDTree,UDTree) -> (UDTree,UDTree)
bestAlignment d pts = pts  
{-  where 
   fertility  = (-) (length . allnodes . fst) (length . allnodes . snd)
   pls        = zip pts (map fertility pts) 
   spts       = sortBy (\ (_,m) (_,n) -> compare n m) pls 
-}
