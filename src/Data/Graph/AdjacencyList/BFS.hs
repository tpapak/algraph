{-|
Module      : Data.Graph.AdjacencyList.BFS
Description : Breadth-first search on adjacency-list graphs
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

Breadth-first search (BFS) for directed graphs represented as adjacency lists.
Provides two entry points:

* 'bfs' — BFS on a concrete 'Graph' value
* 'adjBFS' — BFS on an implicit graph given as an @IntMap [Vertex]@ adjacency map

Both produce a 'BFS' record containing the level (distance) of every reachable
vertex, the BFS parent map, and a topological ordering of the visited vertices.

Used by the Tide algorithm ('Data.Graph.AdjacencyList.PushRelabel.Pure') in the
@globalRelabel@ step to compute vertex heights from distances to the source and
sink in the residual graph.
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Graph.AdjacencyList.BFS
  ( -- * BFS result
    BFS (..)
    -- * Running BFS
  , bfs
  , adjBFS
    -- * Utilities
  , spanningTree
  ) where

import Data.List
import Data.Tuple
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList

-- | Result of a breadth-first search from a single source vertex.
data BFS = BFS { frontier :: Set.IntSet
                 -- ^ Current frontier (vertices at the deepest explored level).
                 -- Empty when the search is complete.
               , level :: IM.IntMap Int
                 -- ^ Map from vertex to its BFS level (shortest distance from source).
               , parent :: IM.IntMap Vertex
                 -- ^ Map from vertex to its BFS parent.
                 -- The source vertex has no entry.
               , maxLevel :: Int
                 -- ^ Maximum level reached during the search.
               , topSort :: [Vertex]
                 -- ^ Vertices in BFS visit order.
                 -- For DAGs this coincides with a topological sort.
               } deriving (Eq, Show)

-- | Initial BFS state with only the source vertex in the frontier.
initialBFS :: Vertex -> BFS
initialBFS s = BFS { frontier = Set.singleton s
                   , level = IM.fromList [(s,0)]
                   , parent= IM.empty
                   , maxLevel = 0
                   , topSort = []
                   }

-- | Run BFS on a 'Graph' from the given source vertex.
--
-- Explores all vertices reachable from @s@ via the graph's 'neighbors'
-- function. Returns a 'BFS' record with levels, parents, and visit order.
--
-- If @s@ is not in the graph's vertex set, returns the initial (empty) BFS.
bfs :: Graph -> Vertex -> BFS
bfs g s = 
  let vset = Set.fromList (vertices g)
      sbfs = initialBFS s
      breadthFirstSearch b =
        if Set.null (frontier b) || not (Set.member s vset)
           then b { topSort = reverse (topSort b) }
           else
             let oldLevel = maxLevel b
                 newLevel = oldLevel + 1
                 oldLevels = level b
                 oldFrontiers = frontier b
                 -- Collect (neighbor, parent) pairs; use IntMap to deduplicate
                 -- and keep only newly discovered vertices in one pass
                 newParMap = Set.foldl'
                   (\acc v ->
                     foldl' (\acc' n ->
                       if IM.member n oldLevels || IM.member n acc'
                         then acc'
                         else IM.insert n v acc'
                     ) acc (neighbors g v)
                   ) IM.empty oldFrontiers
                 newFrontiers = IM.keysSet newParMap
                 newParents = IM.union (parent b) newParMap
                 newLevels = Set.foldl' 
                           (\ac v -> IM.insert v newLevel ac) 
                           oldLevels newFrontiers
                 -- Prepend frontier to topSort (reversed at the end)
                 newTopSort = Set.foldl' (flip (:)) (topSort b) oldFrontiers
                 bbfs = breadthFirstSearch (b { frontier = newFrontiers
                                              , level = newLevels 
                                              , parent = newParents
                                              , maxLevel = newLevel
                                              , topSort = newTopSort
                                            })
               in bbfs
   in breadthFirstSearch sbfs

-- | Run BFS on an implicit graph defined by an adjacency map.
--
-- @adjBFS neimap s@ performs BFS from vertex @s@ where the neighbors of
-- each vertex are given by @neimap :: IntMap [Vertex]@.  Vertices not
-- present in @neimap@ are treated as having no outgoing edges.
--
-- This is used by 'Data.Graph.AdjacencyList.PushRelabel.Internal.residualDistances'
-- to run BFS on the residual graph (whose edge set changes each tide)
-- without constructing a full 'Graph' value.
adjBFS :: IM.IntMap [Vertex] -> Vertex -> BFS
adjBFS neimap s = let b = breadthFirstSearch sbfs
                  in b { topSort = reverse (topSort b) }
  where neighbors v = case IM.lookup v neimap of
                        Nothing -> []
                        Just ns -> ns
        sbfs = initialBFS s
        breadthFirstSearch b
          | Set.null (frontier b) = b
          | otherwise = bbfs
            where oldLevel = maxLevel b
                  newLevel = oldLevel + 1
                  oldLevels = level b
                  oldFrontiers = frontier b
                  -- Collect new vertices; use IntMap to deduplicate
                  newParMap = Set.foldl'
                    (\acc v ->
                      foldl' (\acc' n ->
                        if IM.member n oldLevels || IM.member n acc'
                          then acc'
                          else IM.insert n v acc'
                      ) acc (neighbors v)
                    ) IM.empty oldFrontiers
                  newFrontiers = IM.keysSet newParMap
                  newParents = IM.union (parent b) newParMap
                  newLevels = Set.foldl' 
                                 (\ac v -> IM.insert v newLevel ac) 
                                 oldLevels newFrontiers
                  newTopSort = Set.foldl' (flip (:)) (topSort b) oldFrontiers
                  bbfs = breadthFirstSearch (b { frontier = newFrontiers
                             , level = newLevels 
                             , parent = newParents
                             , maxLevel = newLevel
                             , topSort = newTopSort
                             })

-- | Extract the BFS spanning tree as a list of edges.
--
-- Each edge @(parent, child)@ in the returned list corresponds to one
-- entry in the 'parent' map.
spanningTree :: BFS -> [Edge]
spanningTree b = 
  map (fromTuple . swap) $ IM.toList $ parent b
