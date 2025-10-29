module Utils (
  readInt,
  readIntList,
  randomList
) where

import System.IO
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad
import System.Random

readInt :: String -> IO Int
readInt msg = do
  putStr msg
  hFlush stdout
  line <- getLine
  case readMaybe line of
    Just n -> return n
    Nothing -> do
      putStrLn " Invalid number. Try again."
      readInt msg

readIntList :: IO [Int]
readIntList = do
  putStrLn "Enter numbers separated by spaces:"
  line <- getLine
  let xs = mapMaybe (readMaybe :: String -> Maybe Int) (words line)
  if null xs
    then do
      putStrLn " No valid numbers entered. Try again."
      readIntList
    else return xs

randomList :: Int -> Int -> Int -> IO [Int]
randomList n a b = replicateM n (randomRIO (a, b))
