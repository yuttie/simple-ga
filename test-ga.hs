module Main (main) where

import           Control.Monad.Random
import           Data.Array.IArray    (Array)
import qualified Data.Array.IArray    as A
import           Data.List
import           Data.Ord

import           GA


newtype GeneCode = GeneCode Int
                 deriving (Eq, Ord, Show)

instance Random GeneCode where
    randomR (GeneCode l, GeneCode u) g = (GeneCode x, g')
      where
        (x, g') = randomR (max 1 l, min 10 u) g
    random g = (GeneCode x, g')
      where
        (x, g') = randomR (1, 10) g

main :: IO ()
main = do
    cs <- populateArray 10 (1, 10) (GeneCode 1, GeneCode 10) :: IO [Array Int GeneCode]
    loop (1::Int) cs
  where
    loop n cs = do
        cs' <- evolve env cs
        print (n, maximumBy (comparing fst) $ zip (map eval cs') cs')
        loop (n + 1) cs'
    env = Environment
        { envCrossoverProb = 1.0
        , envMutationProb  = 0.01
        , envFitnessFunc   = eval
        }
    eval :: Array Int GeneCode -> Double
    eval c = exp $ negate $ fromIntegral $ sum $ map (\(i, GeneCode e) -> abs $ i - e) $ A.assocs c
