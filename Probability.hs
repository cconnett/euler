module Probability
    where

import Data.Ratio
import Data.List
import Control.Monad
import Control.Arrow

equating f a b = f a == f b


-- The Monad class is defined without any context on the data type,
-- therefore I cannot have Ord in the context for the wrapped type,
-- and thus cannot sort or reduce in any way the intermediate list of
-- outcomes.  The best I can do is reduce it once at the end =(.
data Probability a = Probability [(a, Rational)]
                     deriving (Show)
fromProbability (Probability aps) = aps

{- What I'd like:
instance Monad (Probability a) where
    return x = Probability [(x, 1%1)]
    Probability aps >>= f = reduce $
                            map (\(a, p) -> map ((second (*p)) . fromProbability) (f a)) aps
        where reduce aps = Probability $
                  map (\group -> (fst (head group), sum (map snd group))) $
                  groupBy (equating fst) $ sort aps
-}
-- Settle for:
instance Monad Probability where
    return x = Probability [(x, 1%1)]
    Probability aps >>= f = Probability $ concatMap (\(a, p) -> ((map (second (*p))) . fromProbability) (f a)) aps

--Probability aps >>= f = map (\(a, p) -> map ((second (*p)) . fromProbability) (f a)) aps
reduceProbability (Probability aps) =
    Probability $ map (\group -> (fst (head group), sum (map snd group))) $
                  groupBy (equating fst) $ sort aps
