{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Criterion.Main
import Lib (fib, slowFib)

main :: IO ()
main = defaultMain
  [ bgroup "fib"
      [ bench "slowFib 10" $ whnf slowFib 10
      , bench "slowFib 20" $ whnf slowFib 20
      , bench "fib 10" $ whnf fib 10
      , bench "fib 20" $ whnf fib 20
      , bench "fib 30" $ whnf fib 30
      , bench "fib 40" $ whnf fib 40
      ]
  ]