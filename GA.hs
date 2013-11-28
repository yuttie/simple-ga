{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module GA
  ( Chromosome(..)
  , Environment(..)
  , selectRoulette
  , evolve
  , populateArray
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.Array.IArray    (IArray, Ix)
import qualified Data.Array.IArray    as A
import           Data.Maybe
import           Data.Tuple


class Chromosome cr where
    crossover :: (Functor m, MonadRandom m) => cr -> cr -> m (cr, cr)
    mutate    :: (Functor m, MonadRandom m) => Double -> cr -> m cr

instance (IArray a e, Ix i, Random e) => Chromosome (a i e) where
    crossover c1 c2 = do
        os <- map chooseOpe <$> getRandoms
        let (c1', c2') = unzip $ zipWith ($) os $ zip (A.elems c1) (A.elems c2)
        return (A.listArray (A.bounds c1) c1', A.listArray (A.bounds c2) c2')
      where
        chooseOpe r
          | r < (1/2::Double) = swap
          | otherwise = id

    mutate p c = do
        is <- catMaybes <$> zipWith (\i r -> if r < p then Just i else Nothing) (A.indices c) <$> getRandoms
        rs <- getRandoms
        return $ c A.// zip is rs

data Environment cr = forall a. Real a => Environment
    { envCrossoverProb :: Double
    , envMutationProb  :: Double
    , envFitnessFunc   :: cr -> a
    }

selectRoulette :: (Real a, MonadRandom m)
               => (cr -> a)
               -> [cr]
               -> m cr
selectRoulette f xs = do
    let es = map (realToFrac . f) xs :: [Double]
    let ws = map (/ sum es) es
    let roulette = zip (scanl1 (+) ws) xs
    r <- getRandom
    return $ snd $ head $ dropWhile (not . (r <) . fst) roulette

evolve :: (Chromosome cr, Functor m, MonadRandom m)
       => Environment cr -> [cr] -> m [cr]
evolve env cs = concat <$> replicateM (length cs `div` 2) loop
  where
    loop = do
        parent1 <- case env of
            Environment _ _ f -> selectRoulette f cs
        parent2 <- case env of
            Environment _ _ f -> selectRoulette f cs
        (child1, child2) <- getRandom >>= \r -> if r < (envCrossoverProb env)
            then crossover parent1 parent2
            else return (parent1, parent2)
        child1' <- mutate (envMutationProb env) child1
        child2' <- mutate (envMutationProb env) child2
        return [child1', child2']

populateArray :: (IArray a e, Ix i, Random e, Functor m, MonadRandom m)
              => Int -> (i, i) -> (e, e) -> m [a i e]
populateArray n ixRange codeRange = replicateM n gen
  where
    gen = A.listArray ixRange <$> getRandomRs codeRange
