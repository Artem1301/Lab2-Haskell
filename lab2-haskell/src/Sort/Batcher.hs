module Sort.Batcher
  ( oesort
  , parOESort
  , depthFromK
  , showListInline
  ) where

import GHC.Conc (setNumCapabilities, getNumCapabilities)
import Text.Read (readMaybe)
import Control.DeepSeq (NFData, force)
import Control.Parallel.Strategies (rpar, rseq, runEval)


oddEvenMerge :: Ord a => [a] -> [a] -> [a]
oddEvenMerge xs ys =
  let evens = merge (every2 xs) (every2 ys)
      odds  = merge (every2 (drop 1 xs)) (every2 (drop 1 ys))
      inter = interleave evens odds
  in adjSwap inter
  where
    merge [] bs = bs
    merge as [] = as
    merge (a:as) (b:bs)
      | a <= b    = a : merge as (b:bs)
      | otherwise = b : merge (a:as) bs

    every2 zs = [x | (x,i) <- zip zs [0..], even i]

    interleave (e:es) (o:os) = e : o : interleave es os
    interleave es []         = es
    interleave [] os         = os

    adjSwap []       = []
    adjSwap [x]      = [x]
    adjSwap (x:rest) = x : pairSwap rest
      where
        pairSwap (a:b:rs)
          | a <= b    = a : b : pairSwap rs
          | otherwise = b : a : pairSwap rs
        pairSwap rs = rs


oesort :: Ord a => [a] -> [a]
oesort []  = []
oesort [x] = [x]
oesort xs  =
  let (as, bs) = split xs
      sa = oesort as
      sb = oesort bs
  in oddEvenMerge sa sb
  where
    split =
      foldr
        (\(i,v) (l,r) -> if even i then (v:l, r) else (l, v:r))
        ([],[])
      . zip [0..]


parOESort :: (NFData a, Ord a) => Int -> [a] -> [a]
parOESort _ []  = []
parOESort _ [x] = [x]
parOESort depth xs
  | depth <= 0 = oesort xs
  | otherwise  =
      let (as, bs) = split xs
      in runEval $ do
           a <- rpar (force (parOESort (depth-1) as))
           b <- rpar (force (parOESort (depth-1) bs))
           rseq a
           rseq b
           pure (oddEvenMerge a b)
  where
    split =
      foldr
        (\(i,v) (l,r) -> if even i then (v:l, r) else (l, v:r))
        ([],[])
      . zip [0..]


depthFromK :: Int -> Int
depthFromK k
  | k <= 1    = 0
  | otherwise = floor (logBase 2 (fromIntegral k :: Double))


showListInline :: Show a => [a] -> String
showListInline = unwords . map show
