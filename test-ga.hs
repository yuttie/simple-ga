module Main (main) where

import           Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import           Data.List
import           Data.Ord

import           GA


main :: IO ()
main = do
    cs <- populate 10 repr :: IO [Array Int Int]
    loop (1::Int) cs
  where
    loop n cs = do
        cs' <- evolve repr env cs
        print (n, maximumBy (comparing fst) $ zip (map eval cs') cs')
        loop (n + 1) cs'
    repr = Representation
        { reprIxRange   = (1, 10)
        , reprCodeRange = (1, 10)
        }
    env = Environment
        { envCrossoverProb = 1.0
        , envMutationProb  = 0.01
        , envFitnessFunc   = eval
        }
    eval :: Array Int Int -> Double
    eval c = exp $ negate $ fromIntegral $ sum $ map (\(i, e) -> abs $ i - e) $ A.assocs c
