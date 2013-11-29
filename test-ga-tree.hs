module Main (main) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.List
import           Data.Ord
import           Data.Ratio

import           GA


data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Lit Rational
         deriving (Eq, Show)

countTerms :: Exp -> Int
countTerms (Add e1 e2) = countTerms e1 + countTerms e2
countTerms (Sub e1 e2) = countTerms e1 + countTerms e2
countTerms (Mul e1 e2) = countTerms e1 + countTerms e2
countTerms (Div e1 e2) = countTerms e1 + countTerms e2
countTerms (Lit _) = 1

data ExpValue = Value Rational
              | Inf

evalExp :: Exp -> ExpValue
evalExp e = case e of
    (Add e1 e2) -> appOpe (+) e1 e2
    (Sub e1 e2) -> appOpe (-) e1 e2
    (Mul e1 e2) -> appOpe (*) e1 e2
    (Div e1 e2) -> appDiv e1 e2
    (Lit r) -> Value r
  where
    appOpe op e1 e2 = case (evalExp e1, evalExp e2) of
      (Inf, _) -> Inf
      (_, Inf) -> Inf
      (Value v1, Value v2) -> Value $ v1 `op` v2
    appDiv e1 e2 = case (evalExp e1, evalExp e2) of
      (Inf, _) -> Inf
      (_, Inf) -> Inf
      (_, Value 0) -> Inf
      (Value v1, Value v2) -> Value $ v1 / v2

instance Random Exp where
    randomR = undefined
    random g = runRand randExp g
      where
        randExp = do
            r <- getRandom
            if r < (1/2::Double)
                then Lit <$> fromIntegral <$> getRandomR (0, 9::Int)
                else do
                    r' <- getRandom
                    case () of
                        _ | r' < (1/4::Double) -> Add <$> randExp <*> randExp
                          | r' < (2/4::Double) -> Sub <$> randExp <*> randExp
                          | r' < (3/4::Double) -> Mul <$> randExp <*> randExp
                          | otherwise          -> Div <$> randExp <*> randExp

instance Chromosome Exp where
    crossover e1 e2 = return (e1, e2)

    mutate p e = do
        r <- getRandom
        if r < p
            then getRandom
            else case e of
                Add e1 e2   -> do e1' <- mutate p e1; e2' <- mutate p e2; return $ Add e1' e2'
                Sub e1 e2   -> do e1' <- mutate p e1; e2' <- mutate p e2; return $ Sub e1' e2'
                Mul e1 e2   -> do e1' <- mutate p e1; e2' <- mutate p e2; return $ Mul e1' e2'
                Div e1 e2   -> do e1' <- mutate p e1; e2' <- mutate p e2; return $ Div e1' e2'
                lit@(Lit _) -> return lit

populate :: (Functor m, MonadRandom m) => Int -> m [Exp]
populate n = replicateM n getRandom

main :: IO ()
main = do
    cs <- populate 10
    loop (1::Int) cs
  where
    loop n cs = do
        cs' <- evolve env cs
        let (v, exp) = maximumBy (comparing fst) $ zip (map eval cs') cs'
        print (n, v, exp)
        loop (n + 1) cs'
    env = Environment
        { envCrossoverProb = 1.0
        , envMutationProb  = 0.01
        , envFitnessFunc   = eval
        }
    eval :: Exp -> Double
    eval e = case evalExp e of
        Inf -> 0
        Value v -> fromRational $ recip $ d * n
          where
            d = abs $ v - 11
            n = fromIntegral $ countTerms e
