module Main where

import Control.Monad
import Data.Time.Clock
import System.IO
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Data.Bits ((.&.))
import GHC.Conc (setNumCapabilities)

import Sort.Batcher (oesort, parOESort, depthFromK, showListInline)


readIntSafe :: String -> IO Int
readIntSafe msg = do
  putStr msg
  hFlush stdout
  s <- getLine
  case reads s of
    [(x, "")] -> return x
    _         -> putStrLn " Invalid number, try again." >> readIntSafe msg


readIntList :: IO [Int]
readIntList = do
  putStrLn "Enter space-separated integers:"
  putStr "> "
  hFlush stdout
  s <- getLine
  let ws = words s
  case traverse readMaybe ws of
    Just xs -> return xs
    Nothing -> putStrLn "Invalid list, try again." >> readIntList


randomList :: Int -> Int -> Int -> IO [Int]
randomList n a b = replicateM n (randomRIO (a, b))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "========== Parallel Odd-Even Mergesort =========="
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
      n <- readIntSafe "Enter array length: "
      a <- readIntSafe "Min value: "
      b <- readIntSafe "Max value: "
      arr <- randomList n a b
      putStrLn $ "Generated: " ++ showListInline arr
      menuLoop (Just arr) threads

    "3" -> do
      t <- readIntSafe "Enter number of threads: "
      setNumCapabilities (max 1 t)
      putStrLn $ " Threads set to " ++ show t
      menuLoop current (max 1 t)

    "4" -> case current of
      Nothing -> do
        putStrLn " No array provided yet!"
        menuLoop current threads

      Just arr -> do
        let n = length arr
            isPow2 x = x > 0 && (x .&. (x - 1)) == 0

        if not (isPow2 n)
          then do
            putStrLn $ " Error: array length = " ++ show n ++
                       ", but must be a power of two (2^k)."
            putStrLn " Please enter or generate a valid array."
            menuLoop current threads
          else do
            let depth = depthFromK threads

            putStrLn $ "\nSorting with " ++ show threads ++ " threads..."
            start <- getCurrentTime

            let result =
                  if threads <= 1
                    then oesort arr
                    else parOESort depth arr

            end <- getCurrentTime

            putStrLn $ "\n Sorted: " ++ showListInline result
            putStrLn $ " Time: " ++ show (diffUTCTime end start)
            menuLoop (Just result) threads

    "5" -> putStrLn " Exiting. Goodbye!"
    _   -> do
      putStrLn " Invalid choice."
      menuLoop current threads
