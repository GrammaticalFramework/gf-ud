module Main where

import PGF

import System.Environment (getArgs)
import Control.Concurrent
import Control.Monad

-- to get parallel processing:
-- Build with -threaded -rtsopts
-- Run with +RTS -N$cores -RTS

main = do
  xx <- getArgs
  case xx of
    pgfile:lang:infile:_ -> do
      pgf <- readPGF pgfile
      ls <- readFile infile >>= return . lines
      rs <- parparse pgf lang ls
      return ()
    _ -> putStrLn "usage: parparse pgf lan infile +RTS -N6 -RTS"

parparse pgf lang = manyLater (putStrLn . prse)
 where
  prse s = case parse pgf (mkCId lang) (startCat pgf) s of
    t:_ -> showExpr [] t ++ "\t" ++ s
    _ -> "NO PARSE:\t" ++ s

-- from Anton Ekblad 2020-01-09
manyLater :: (a -> IO b) -> [a] -> IO [b]
manyLater f chunks = do
  vs <- forM chunks $ \chunk -> do
    v <- newEmptyMVar
    forkIO $ do
      x <- f chunk
      x `seq` putMVar v x
    return v
  mapM takeMVar vs
