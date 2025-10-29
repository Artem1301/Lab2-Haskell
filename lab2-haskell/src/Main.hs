module Main where

import Control.Monad
import Data.Time.Clock
import System.IO
import qualified Data.Vector.Mutable as MV
import Sort.Batcher
import Utils

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "========== Parallel Batcher Sort =========="
  menuLoop Nothing 4

menuLoop :: Maybe [Int] -> Int -> IO ()
menuLoop current threads = do
  putStrLn "\nMenu:"
  putStrLn "1. Enter array manually"
  putStrLn "2. Generate random array"
  putStrLn "3. Set number of threads"
  putStrLn "4. Run sorting"
  putStrLn "5. Exit"
  putStr "Choose option: "
  hFlush stdout
  choice <- getLine
  case choice of
    "1" -> do
      arr <- readIntList
      menuLoop (Just arr) threads
    "2" -> do
      n <- readInt "Enter array length: "
      a <- readInt "Min value: "
      b <- readInt "Max value: "
      arr <- randomList n a b
      putStrLn $ "Generated: " ++ show arr
      menuLoop (Just arr) threads
    "3" -> do
      t <- readInt "Enter number of threads: "
      putStrLn $ " Threads set to " ++ show t
      menuLoop current (max 1 t)
    "4" -> case current of
      Nothing -> do
        putStrLn " No array provided yet!"
        menuLoop current threads
      Just arr -> do
        let n0 = length arr
        let p = nextPow2 n0
        let padded = padWith p arr
        v <- fromListIO padded

        putStrLn $ "\nSorting with " ++ show threads ++ " threads..."
        start <- getCurrentTime
        bitonicSort threads v p
        end <- getCurrentTime

        sortedList <- forM [0 .. p - 1] $ \i -> MV.read v i
        let result = trimResult sortedList

        putStrLn $ "\n Sorted: " ++ show result
        putStrLn $ " Time: " ++ show (diffUTCTime end start)
        menuLoop (Just result) threads
    "5" -> putStrLn " Exiting. Goodbye!"
    _   -> do
      putStrLn " Invalid choice."
      menuLoop current threads
