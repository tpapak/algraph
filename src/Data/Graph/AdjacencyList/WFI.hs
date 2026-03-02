{-|
Module      : Data.Graph.AdjacencyList.WFI
Description : Floyd-Warshall all-pairs shortest paths
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

Implementation of the
<https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm Floyd-Warshall algorithm>
for computing all-pairs shortest path distances on a weighted or unweighted
directed graph.  Complexity: O(V^3).
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Graph.AdjacencyList.WFI
  ( Distances (..)
  , Weight
  , shortestDistances
  , unweightedShortestDistances
  , adjacencyArray
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map   as M
import qualified Data.IntMap   as IM

import Data.Graph.AdjacencyList

-- | In an unweighted graph the weight is 1 for each edge
type Weight = Rational

type IMArray = IM.IntMap (IM.IntMap Weight)
-- | The array containing the distances from vertex to vertex
newtype Distances = Distances IMArray
  deriving (Eq, Ord, Read)

instance Show Distances where
  show (Distances d) =
    let vs = IM.keys d
     in show d

-- | Reads distance array. Nothing corresponds to infinite distance
shortestDistance :: IMArray -> Vertex -> Vertex -> Maybe Weight
shortestDistance dists u v = do
  vmap <- IM.lookup u dists
  IM.lookup v vmap

-- | Build the initial distance matrix from a graph's edges (unit weights).
-- Self-distances are 0; direct edges have distance 1; all others are absent
-- (infinite).  Pass the result to 'shortestDistances' to run Floyd-Warshall.
adjacencyArray :: Graph -> Distances
adjacencyArray g =
  let es = edges g
      dists = foldl' (\dists (Edge u v) ->
        let vmap = case IM.lookup u dists of 
                      Nothing -> IM.empty
                      Just vmap' -> vmap'
         in IM.insert u ((IM.insert v 1) vmap) dists
                ) IM.empty es
   in Distances $ IM.mapWithKey (\i m -> IM.insert i 0 m) dists 

-- | Get all shortest distances given initial weights on edges
shortestDistances :: Graph -> Distances -> Distances
shortestDistances g (Distances dists) = Distances $ foldl' update dists vs
  where
    vs = vertices g
    update d k = IM.mapWithKey shortmap d
      where
        shortmap :: Vertex -> IM.IntMap Weight -> IM.IntMap Weight
        shortmap i jmap = foldr shortest IM.empty vs
          where shortest j m =
                  case (old,new) of
                    (Nothing, Nothing) -> m
                    (Nothing, Just w ) -> IM.insert j w m
                    (Just w, Nothing) -> IM.insert j w m
                    (Just w1, Just w2) -> IM.insert j (min w1 w2) m
                  where
                    old = IM.lookup j jmap
                    new = do w1 <- shortestDistance d i k
                             w2 <- shortestDistance d k j
                             return (w1+w2)

-- | Get all shortest unweighted distances
unweightedShortestDistances :: Graph -> Distances
unweightedShortestDistances g = shortestDistances g (adjacencyArray g)
