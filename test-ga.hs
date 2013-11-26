{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.Array.IArray   (IArray, Ix, Array)
import qualified Data.Array.IArray   as A
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Tuple


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
       -> a i e
       -> m (a i e)
mutate p c = do
    is <- catMaybes <$> zipWith (\i r -> if r < p then Just i else Nothing) (A.indices c) <$> getRandoms
    rs <- getRandoms
    return $ c A.// zip is rs

evolve :: (IArray a e, Ix i, Random e, Fractional b, Ord b, Random b, Functor m, MonadRandom m)
       => (a i e -> b) -> [a i e] -> m [a i e]
evolve f cs = concat <$> replicateM (length cs `div` 2) loop
  where
    probCrossover = 1.0 :: Double
    probMutation = 0.01 :: Double
    loop = do
        parent1 <- selectRoulette f cs
        parent2 <- selectRoulette f cs
        (child1, child2) <- getRandom >>= \r -> if r < probCrossover
            then crossoverUniform parent1 parent2
            else return (parent1, parent2)
        child1' <- mutate probMutation child1
        child2' <- mutate probMutation child2
        return [child1', child2']

populate :: (IArray a e, Ix i, Random e, Functor m, MonadRandom m)
         => Int -> (i, i) -> m [a i e]
populate n r = replicateM n gen
  where
    gen = A.listArray r <$> getRandoms

newtype GeneValue = GeneValue Int
                  deriving (Eq, Ord, Show)

instance Random GeneValue where
    randomR (GeneValue l, GeneValue u) g = (GeneValue x, g')
      where
        (x, g') = randomR (max 1 l, min 10 u) g
    random g = (GeneValue x, g')
      where
        (x, g') = randomR (1, 10) g

main :: IO ()
main = do
    cs <- populate 10 (1, 10) :: IO [Array Int GeneValue]
    loop (1::Int) cs
  where
    loop n cs = do
        cs' <- evolve eval cs
        print (n, maximumBy (comparing fst) $ zip (map eval cs') cs')
        loop (n + 1) cs'
    eval :: Array Int GeneValue -> Double
    eval c = exp $ negate $ fromIntegral $ sum $ map (\(i, GeneValue e) -> abs $ i - e) $ A.assocs c
