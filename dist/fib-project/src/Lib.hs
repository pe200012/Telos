module Lib (fib, slowFib, drawFibGraph) where

import           Control.Parallel.Strategies


-- Draw an ASCII graph of Fibonacci numbers
drawFibGraph :: Integer -> IO ()
drawFibGraph limit = do
  let fibValues = map fib [0 .. limit]
      graphLine n val = show n ++ ": " ++ replicate (fromInteger val `mod` 50) '*' -- Limit bar width for readability
  mapM_ (\(n, val) -> putStrLn (graphLine n val)) (zip [0 ..] fibValues)


fib :: Integer -> Integer
fib n = fibMatrix n
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 1) + slowFib (n - 2)


fibMatrix :: Integer -> Integer
fibMatrix n
  | n == 0 = 0
  | otherwise
    = let
        base   = ( ( 1, 1 ), ( 1, 0 ) )
        result = matrixPower base (n - 1)
      in 
        fst (fst result)

matrixMultiply :: ( ( Integer, Integer ), ( Integer, Integer ) )
               -> ( ( Integer, Integer ), ( Integer, Integer ) )
               -> ( ( Integer, Integer ), ( Integer, Integer ) )
matrixMultiply ( ( a, b ), ( c, d ) ) ( ( e, f ), ( g, h ) )
  = let
      p1 = (a * e + b * g)
      p2 = (a * f + b * h)
      p3 = (c * e + d * g)
      p4 = (c * f + d * h)
    in 
      runEval $ do
        p1' <- rpar p1
        p2' <- rpar p2
        p3' <- rpar p3
        p4' <- rpar p4
        _ <- rseq p1'
        _ <- rseq p2'
        _ <- rseq p3'
        _ <- rseq p4'
        return ( ( p1', p2' ), ( p3', p4' ) )

matrixPower :: ( ( Integer, Integer ), ( Integer, Integer ) )
            -> Integer
            -> ( ( Integer, Integer ), ( Integer, Integer ) )
matrixPower matrix 0 = ( ( 1, 0 ), ( 0, 1 ) ) -- Identity matrix
matrixPower matrix n
  | even n = runEval $ do
    halfPower <- rpar (matrixPower matrix (n `div` 2))
    rseq halfPower
    return (matrixMultiply halfPower halfPower)
  | otherwise = matrixMultiply matrix (matrixPower matrix (n - 1))
