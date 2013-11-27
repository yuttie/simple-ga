{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module GA
  ( Environment(..)
  , Representation(..)
  , selectRoulette
  , crossoverUniform
  , mutate
  , evolve
  , populate
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.Array.IArray    (IArray, Ix)
import qualified Data.Array.IArray    as A
import           Data.Maybe
import           Data.Tuple


data Environment cr a = Environment
    { envCrossoverProb :: Double
    , envMutationProb  :: Double
    , envFitnessFunc   :: cr -> a
    }

data Representation i e = Representation
    { reprIxRange   :: (i, i)
    , reprCodeRange :: (e, e)
    }

selectRoulette :: (Fractional b, Ord b, Random b, MonadRandom m)
               => (a -> b)
               -> [a]
               -> m a
selectRoulette f xs = do
    let es = map f xs
    let ws = map (/ sum es) es
    let roulette = zip (scanl1 (+) ws) xs
    r <- getRandom
    return $ snd $ head $ dropWhile (not . (r <) . fst) roulette

crossoverUniform :: (IArray a e, Ix i, Functor m, MonadRandom m)
                 => a i e
                 -> a i e
                 -> m (a i e, a i e)
crossoverUniform c1 c2 = do
    os <- map chooseOpe <$> getRandoms
    let (c1', c2') = unzip $ zipWith ($) os $ zip (A.elems c1) (A.elems c2)
    return (A.listArray (A.bounds c1) c1', A.listArray (A.bounds c2) c2')
  where
    chooseOpe r
      | r < (1/2::Double) = swap
      | otherwise = id

mutate :: (IArray a e, Ix i, Random e, Functor m, MonadRandom m)
       => Double
       -> (e, e)
       -> a i e
       -> m (a i e)
mutate p range c = do
    is <- catMaybes <$> zipWith (\i r -> if r < p then Just i else Nothing) (A.indices c) <$> getRandoms
    rs <- getRandomRs range
    return $ c A.// zip is rs

evolve :: (IArray a e, Ix i, Random e, Fractional b, Ord b, Random b, Functor m, MonadRandom m)
       => Representation i e -> Environment (a i e) b -> [a i e] -> m [a i e]
evolve repr env cs = concat <$> replicateM (length cs `div` 2) loop
  where
    loop = do
        parent1 <- selectRoulette (envFitnessFunc env) cs
        parent2 <- selectRoulette (envFitnessFunc env) cs
        (child1, child2) <- getRandom >>= \r -> if r < (envCrossoverProb env)
            then crossoverUniform parent1 parent2
            else return (parent1, parent2)
        child1' <- mutate (envMutationProb env) (reprCodeRange repr) child1
        child2' <- mutate (envMutationProb env) (reprCodeRange repr) child2
        return [child1', child2']

populate :: (IArray a e, Ix i, Random e, Functor m, MonadRandom m)
         => Int -> Representation i e -> m [a i e]
populate n repr = replicateM n gen
  where
    gen = A.listArray (reprIxRange repr) <$> getRandomRs (reprCodeRange repr)
