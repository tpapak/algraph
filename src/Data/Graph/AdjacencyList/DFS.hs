{-|
Module      : Data.Graph.AdjacencyList.DFS
Description : Depth-first search with topological sort and longest path
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

Depth-first search (DFS) on directed graphs.  Produces a topological ordering,
a visited-order list, and the set of discovered vertices.  Also provides
'longestPath' on DAGs and connectivity queries.
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}


module Data.Graph.AdjacencyList.DFS
  ( DFS (..)
  , dfs
  -- * Types
  , DAG
  , Distances
  -- * get longest path from a vertex to another
  , longestPath
  , postordering
  , areConnected
  , distances
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM
import qualified Data.IntSet   as Set
import qualified Data.Sequence as Seq

import Data.Graph.AdjacencyList

-- | Result of a depth-first search from a single source vertex.
data DFS = DFS { topsort :: [Vertex]
                 -- ^ Vertices in reverse post-order (topological sort for DAGs).
               , visited :: [Vertex]
                 -- ^ Vertices in DFS visit order.
               , discovered   :: Set.IntSet
                 -- ^ Set of all discovered vertices.
               , called :: Int
                 -- ^ Number of DFS calls made.
               } deriving (Eq, Show)

initialDFS :: DFS
initialDFS = DFS { topsort = []
                 , discovered   = Set.empty
                 , visited = []
                 , called = 0
                 }

-- | Depth first search
dfs :: Graph -> Vertex -> DFS
dfs g s = 
  let vset = Set.fromList (vertices g)
  in if not $ Set.member s vset
     then initialDFS
     else
       let depthFirstSearch :: Vertex -> DFS -> DFS
           depthFirstSearch v ac
              | Set.member v (discovered ac) = ac
              | otherwise =
              let -- Mark v as discovered BEFORE recursing (prevents revisits in cyclic graphs)
                  ac0 = ac { discovered = Set.insert v (discovered ac) }
                  ns = neighbors g v
                  !ac' = foldl' (\ac'' n -> if not (Set.member n (discovered ac''))
                                              then depthFirstSearch n ac''
                                              else ac''
                                ) ac0 ns
                  res = ac' { topsort = v : topsort ac'
                            -- Prepend to visited (reversed at end)
                            , visited = v : visited ac'
                            , called = called ac' + 1
                            }
               in res
           result = depthFirstSearch s initialDFS
        in result { visited = reverse (visited result) }

-- | Post-order traversal (reverse of 'topsort').
postordering :: DFS -> [Vertex]
postordering = reverse . topsort

-- | :)
type DAG = Graph

-- | Ginen a DAG and a vertex you get the distances
distances' :: DAG  -> Vertex -> IM.IntMap Vertex
distances' g s =
  let topsorted = topsort $ dfs g s
      initdists = foldl' (\ac v -> IM.insert v 0 ac) IM.empty $ vertices g
   in foldl' (\ac v -> 
        let neis = neighbors g v
            distv = case IM.lookup v ac of
                      Nothing -> 0
                      Just d -> d
         in foldl' (\dists' nei -> 
           let neidist = case IM.lookup nei dists' of
                           Nothing -> 0
                           Just nd -> nd
               newdist = max neidist (distv+1)
            in IM.insert nei newdist dists'
                  ) ac neis
      ) initdists topsorted

-- | Map from vertex to its distance (number of edges) from the source in a 'DAG'.
type Distances = IM.IntMap Vertex

-- | Ginen a DAG and a vertex you get the distances
distances :: DAG  -> DFS -> Vertex -> Distances
distances g dfs' s =
  let topsorted = topsort $ dfs'
      !initdists = foldl' (\ac v -> IM.insert v 0 ac) IM.empty $ vertices g
   in foldl' (\ac v -> 
        let neis = neighbors g v
            distv = case IM.lookup v ac of
                      Nothing -> 0
                      Just d -> d
         in foldl' (\dists' nei -> 
           let neidist = case IM.lookup nei dists' of
                           Nothing -> 0
                           Just nd -> nd
               newdist = max neidist (distv+1)
            in IM.insert nei newdist dists'
                  ) ac neis
      ) initdists topsorted

type TopologicalSorting = [Vertex]
-- |checks if s is predecessor of t
dependsOn :: TopologicalSorting -> Vertex -> Vertex -> Bool
dependsOn topsorted t s = elem t (snd (span ((==) s) topsorted))

-- | Check whether vertex @v@ is reachable from vertex @u@ according to the
-- given distance map (distance > 0 means reachable; @u@ is reachable from itself).
areConnected :: Distances -> Vertex -> Vertex -> Bool
areConnected dists u v = (fromJust $ IM.lookup v dists) > 0 || v == u

-- |Longest path from tail to nose
longestPath :: Graph -> Vertex -> Vertex -> [Edge]
longestPath g s t =
  let dfs' = dfs g s
      topsorted = topsort dfs'
      dists = distances g dfs' s
      revg = reverseGraph g
      disconnected = filter (\n -> not (areConnected dists s n)) $ vertices g
   in if not $ dependsOn topsorted t s
     then []
     else 
       if not $ null disconnected
          then
            let cleangraph = filterVertices (\v -> not $ elem v disconnected) g
             in longestPath cleangraph s t
          else
            let path' :: Vertex -> [Edge] -> [Edge]
                path' v p 
                  | v == s = p
                  | otherwise = 
                         let parents = neighbors revg v
                          in if null parents
                                then []
                                else
                                  if parents == [s]
                                   then (Edge s v):p
                                   else 
                                     let pred :: Vertex
                                         pred = fst $ foldl'
                                           (\(prevmax,maxdist) parent ->
                                             let currentDist =
                                                   case IM.lookup parent dists of
                                                     Nothing -> (0,0)
                                                     Just d -> (parent,d)
                                              in if maxdist < snd currentDist
                                                    then currentDist
                                                    else (prevmax,maxdist)
                                                    ) (0,0) parents
                                      in  path' pred $ (Edge pred v): p
             in path' t []
