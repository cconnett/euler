{-# OPTIONS_GHC -fglasgow-exts #-}

module AStar where
import Control.Monad (liftM2)
--import Control.Monad.Instances
import Data.List (findIndex)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified PriorityQueue2 as Q
import Debug.Trace

tracing = True
history = True --not used currently

astar :: (Num cost, Ord cost, Ord state, Show state) =>
         state ->
        (state -> [state]) ->
        (state -> Bool) ->
        (state -> cost) ->
        (state -> cost) ->
        [state] -- path
astar start succ end cost heur
    = astar' 1 (S.singleton start) (Q.singleton ((cost start + heur start), [start]))
 where
 astar' i seen q
    | Q.isEmpty q  = error "No Solution."
    | end n     = next
    | otherwise =
        (if tracing && i `mod` 5000 == 0 then
             let best = head $ snd $ Q.findMin q in
             trace (show i ++ " " ++ show best ++
                    " " ++ show (cost best, heur best, cost best + heur best)) else
             id) $
        astar' (i+1) seen' q'
  where
  (c, next) = Q.findMin q
  dq = Q.deleteMinKV q
  n     = head next
  succs = filter (`S.notMember` seen) $ succ n
  --costs = map ((+ c) . (subtract $ heur n) . liftM2 (+) cost heur) succs
  costs = map (\successor -> cost successor + heur successor) succs
  q'    = dq `Q.unionKV` Q.fromListKV (zip costs (map (:next) succs))
  seen' = seen `S.union` S.fromList succs
