{-# LANGUAGE ScopedTypeVariables #-}
module Sort.Batcher (
    bitonicSort,
    fromListIO,
    trimResult,
    nextPow2,
    padWith
) where

import Control.Monad
import Control.Concurrent
import qualified Data.Vector.Mutable as MV
import Data.Bits

nextPow2 :: Int -> Int
nextPow2 n
  | n <= 1 = 1
  | popCount n == 1 = n
  | otherwise = let p = until (\x -> x >= n) (*2) 1 in p

padWith :: Int -> [Int] -> [Int]
padWith p xs = xs ++ replicate (p - length xs) (maxBound :: Int)

compareAndSwap :: MV.IOVector Int -> Int -> Int -> IO ()
compareAndSwap v i j = do
  xi <- MV.read v i
  xj <- MV.read v j
  when (xi > xj) $ do
    MV.write v i xj
    MV.write v j xi

parallelForIndices :: Int -> MV.IOVector Int -> Int -> Int -> Int -> IO ()
parallelForIndices threads v n k j = do
  let chunkSize = (n + threads - 1) `div` threads
  doneVars <- forM [0 .. threads - 1] $ \t -> do
    let start = t * chunkSize
        end = min n (start + chunkSize)
    m <- newEmptyMVar
    _ <- forkIO $ do
      let loop i
            | i >= end = putMVar m ()
            | otherwise = do
                let l = i `xor` j
                when (l > i) $ do
                  let ascending = ((i .&. k) == 0)
                  if ascending
                    then compareAndSwap v i l
                    else compareAndSwap v l i
                loop (i + 1)
      loop start
    return m
  mapM_ takeMVar doneVars

bitonicSort :: Int -> MV.IOVector Int -> Int -> IO ()
bitonicSort threads v n = do
  let loopK k
        | k > n = return ()
        | otherwise = do
            let loopJ j
                  | j < 1 = return ()
                  | otherwise = do
                      parallelForIndices threads v n k j
                      loopJ (j `div` 2)
            loopJ (k `div` 2)
            loopK (k * 2)
  loopK 2

trimResult :: [Int] -> [Int]
trimResult = filter (/= (maxBound :: Int))

fromListIO :: [Int] -> IO (MV.IOVector Int)
fromListIO xs = do
  v <- MV.new (length xs)
  forM_ (zip [0..] xs) $ \(i,x) -> MV.write v i x
  return v
