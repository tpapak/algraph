{-|
Module      : PushRelabel - Pure
Description : Maximum Flow - Push relabel - Tide Algorithm
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


= Tide - Push (Pull) Relabel
The tide algorithm is a push relabel implementation for solving the 
 [max flow problem](https://en.wikipedia.org/wiki/Push%E2%80%93relabel_maximum_flow_algorithm#Practical_implementations) 
The algorithm is tested on directed graphs.

=== Definitions
A network \( N \) is defined by a directed graph \( G \) its source and sink \(s,t\) and the capacities \(C : E \rightarrow R^+ \) 
Following push-relabel's terminology the residual graph \(R\) is a network containing both original edges of \(G\) (forward edges) and backward (reverse) edges, with the additional properties of __preflow__ \(F : E_R \rightarrow R^+ \) on the forward edges and residual capacities \(C_R\) on all edges of \(R\) as well as the following properties on the vertices:

* Height \(H: V \rightarrow R^+\) which determines whether preflow can be pushed through an edge.
* Excess \(X:  V \rightarrow R^+\) recording the excess flow of each vertex. 
When the algorithm terminates all excess is \(0\) and the preflow of each edges is
the actual maximum flow of \(N\).
* Level \(L:  V \rightarrow R^+\) is the shortest distance from the source in the original graph \( G \) and therefor is constant during the process. The level is needed to define the order by which flow is pushed.

=== Operations
The main difference in the definitions lies in the split of the PR push operation
into two, depending on whether it is performed in a forward edge or reverse
edge. The former operation is called __push__ (from now on, /push/ will be used in
this context) and the latter __pull__.
Relabel is the usual PR adjusting of heights.

/PR/ guaranties that there is a cut between the source and sink in the residual
graph partitioning \(R\) into \(S\) and \(T\) vertices.

The tide algorithm is iterative and each iteration consists of three steps (tides)

1. global-relabel
  Labels vertices from their distances from source and sink,
  by doing breadth first searches.
  Heights for the source partition vertices is N \+ their distance to the source
  and heights for the sink equal the distance from the sink.
2. global-push
  Pushes flow through all eligible __forward__ edges in the residual graph. 
  The push followes the level of the vertices. That means the push is done from the source to the sink starting from the sink then the vertices with level 1 then those of level 2 and so on.
3. global-pull
  Global pull on the other side is done starting by the sink and going back until we reach the sink pulling (increasing preflow) only on __reverse__ edges.

The order of the global pushes and pulls is always the same and is dictated by the breadth first search of the original graph (which gives the levels). After adequate iterations of these three steps (tides) we reach the maximum flow.

 -}

{-# LANGUAGE BangPatterns #-}

module Data.Graph.AdjacencyList.PushRelabel.Pure where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import Control.Monad

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Internal
import qualified Data.Graph.AdjacencyList.BFS as BFS

-- | Implementation of the push relabel algorithm. Initialize Residual graph
-- from a network and runs the tide algorithm checking for various possible
-- errors in the process.
pushRelabel :: Network -> Either String ResidualGraph
pushRelabel net =
  let initg = initializeResidualGraph net
      res = tide initg 0
      nvs = vertices $ graph $ network res
      s = source net
      t = sink net
      insouts = filter (\v -> v /= s && v /= t && inflow res v < outflow res v) nvs
      xsflows = filter (\v -> v /= s && v /= t && inflow res v - outflow res v /= excess res v) nvs
      ofvs = IM.foldr (\ovs ac -> Set.union ac ovs) Set.empty $ overflowing res
      notofvs = filter (\ ov -> 
                          let (ResidualVertex v l h x) = fromJust (IM.lookup ov (netVertices res)) 
                              ml = (IM.lookup l (overflowing res)) 
                           in case ml of
                                Nothing -> True
                                Just os -> not $ Set.member ov os
                       ) $ Set.toList $ getOverflowing $ netVertices res
      errovfs = Set.filter (\v -> excess res v == 0) ofvs
   in if null insouts && null xsflows && Set.null errovfs && null notofvs
      then Right res
      else 
        if not $ null insouts 
              then Left $ "Error Inflow < Outflow " ++ show insouts
              else
                if not $ null xsflows 
                  then Left $ "Error vertex excess " ++ show xsflows
                  else
                    if not $ Set.null errovfs 
                      then Left $ "Error not really overflowing " ++ show errovfs
                      else Left $ "Error not in overflowing " ++ show notofvs
                        ++ " overflowings are " ++ show (overflowing res)
                        ++ " nevertices are " ++ show (netVertices res)

-- | The main part of the algorithm. It is a recursive algorithm consisting of a
-- global relabel, followed by a global push and then a global pull. When the
-- flow and the overflowing vertices don't change max flow is achieved.
-- Optimization: skip globalRelabel when the residual graph topology did not
-- change in the previous tide (no edge crossed a saturation boundary).
tide :: ResidualGraph -> Int -> ResidualGraph 
tide rg steps = 
  let g = rg `seq` (graph $ network rg)
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      olf = netFlow rg
      -- Only run globalRelabel if the residual topology changed
      relabeled = if topologyChanged rg
                  then globalRelabel rg
                  else rg
      -- Reset flag before push/pull so we detect new changes
      rg0 = relabeled { topologyChanged = False }
      rg' = globalPush $ globalPull rg0 -- then global push and then global pull
      nfl = netFlow rg'
      steps' = steps + 1
      oovfls = overflowing rg
      novfls = overflowing rg'
   in if nfl == olf -- if new flow == old flow 
         then 
           if oovfls == novfls -- and the overflowing nodes didn't change
              then rg' { network = networkFromResidual rg' -- algorithm ends
                       , steps = steps'}
              else tide rg' steps'
         else tide rg' steps'

-- | Pushes flow starting with vertices closer to the source and moving towards
-- the sink. Only forward edges are chosen to increase preflow.
-- The order of vertices picked follows the shortest distance of the vertices from the source in 
-- the original graph. Thus global-push is a __left fold__ on the overflowing vertices.
-- (The overflowing vertices are ordered according to their level)
globalPush :: ResidualGraph -> ResidualGraph 
globalPush rg = 
  let ovfs = overflowing rg
   in IM.foldl' (\ac lset -> 
         Set.foldl' (\ac' v -> pushNeighbors ac' v)
         ac lset
      ) rg ovfs

-- | Global pull is the oposite of the global-push meaning preflow is increased
-- only in reverse (residual) edges and the order of pulls is from the sink to the source.
-- It is a __right fold__ on the overflowing vertices.
globalPull rg = 
  let ovfs = overflowing rg
   in IM.foldr' (\lset ac -> 
         Set.foldl' (\ac' v -> pullNeighbors ac' v)
         ac lset
               ) rg ovfs

-- | Push through all (forward) residual neighbors
pushNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pushNeighbors g v =
  let neimap = netNeighborsMap g
      (fwdMap, _) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) $ IM.keys fwdMap
   in foldl' (\ac e -> 
                let mv = push ac e
                in case mv of 
                    Nothing -> ac
                    Just g'' -> g'') g feds

-- | Push through all (reverse) meaning pull all residual neighbors
pullNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pullNeighbors g v =
  let neimap = netNeighborsMap g
      (_, revMap) = fromJust $ IM.lookup v neimap
      reds = map (\n -> fromTuple (n,v)) $ IM.keys revMap
   in foldl' (\ac e -> 
                let mv = pull ac e
                 in case mv of 
                      Nothing -> ac
                      Just g'' -> g'') g reds

-- | Global relabel according to bfs from source and sink
globalRelabel :: ResidualGraph -> ResidualGraph
globalRelabel rg =
  let g = graph $ network rg
      sh = numVertices g
      (slvs, tlvs) = residualDistances rg
      rg' = IM.foldrWithKey 
              (\ v l ac -> 
                 -- Heights for the source partition vertices is N \+ their distance to the source
                let h = sh + l 
                  in updateHeight ac v h
              ) rg slvs 
   in IM.foldrWithKey (\ v h ac
       -- Heights for the sink partition vertices equals the distance from sink
       -> updateHeight ac v h) 
       rg' tlvs
