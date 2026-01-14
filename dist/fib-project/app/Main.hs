module Main (main) where

import Lib
import System.Environment (getArgs)
import Data.Maybe (listToMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["graph"] -> do
      putStrLn "Drawing Fibonacci graph for first 10 numbers:"
      drawFibGraph 10
    ["graph", nStr] -> case reads nStr of
      [(n, _)] -> do
        putStrLn $ "Drawing Fibonacci graph for first " ++ show n ++ " numbers:"
        drawFibGraph n
      _ -> putStrLn "Invalid number after 'graph'. Please pass an integer."
    _ -> do
      let n = maybe 10 read (listToMaybe args) :: Integer
      print $ map fib [0..(n-1)]
