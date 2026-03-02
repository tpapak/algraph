{-|
Module      : Data.Graph.AdjacencyList.PushRelabel.Pure
Description : Tide algorithm — a push-pull-relabel max-flow solver
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

= Tide — Push (Pull) Relabel

The Tide algorithm is a push-relabel variant for solving the
<https://en.wikipedia.org/wiki/Maximum_flow_problem maximum flow problem>
on directed graphs.

== Definitions

A network \( N = (G, s, t, C) \) consists of a directed graph \( G \),
source \( s \), sink \( t \), and capacities \( C : E \to \mathbb{R}^+ \).

The /residual graph/ \( R \) contains both forward edges (with residual
capacity \( c - f \)) and backward edges (with capacity \( f \)).
Each vertex carries:

* __Height__ \( h(v) \): determines whether flow can be pushed along an edge
  (flow moves from higher to lower height).
* __Excess__ \( x(v) \): records the net surplus of flow at \( v \).
  At termination all excesses are zero and the preflow is a valid max flow.
* __Level__ \( \ell(v) \): the BFS distance from source in the /original/
  graph \( G \).  Constant throughout the algorithm.  Determines the
  sweep order.

== Operations

The key difference from classical push-relabel is that the push operation
is split into two:

* __Push__ (on forward edges): increases flow towards the sink.
* __Pull__ (on reverse edges): decreases flow, effectively pulling excess
  backwards towards the source.

== Algorithm

Each iteration (\"tide\") consists of three global sweeps:

1. __globalRelabel__: BFS from sink (and source) on the residual graph to
   recompute vertex heights.  Source-side vertices get
   \( h = |V| + d_s(v) \); sink-side vertices get \( h = d_t(v) \).

2. __globalPull__: /right fold/ over overflowing vertices in descending
   level order, pulling flow on reverse edges (from sink towards source).

3. __globalPush__: /left fold/ over overflowing vertices in ascending
   level order, pushing flow on forward edges (from source towards sink).

The algorithm terminates when both the net flow and the set of overflowing
vertices are unchanged between consecutive tides.

=== Skip-globalRelabel optimization

When no edge crosses a saturation boundary during push\/pull (the
'topologyChanged' flag is 'False'), the residual graph topology is
unchanged and globalRelabel is skipped.  This saves 1.25--1.61x in
practice.

== Complexity

* Per-tide cost: \( O((V+E) \log V) \) with IntMap data structures.
* Number of tides: \( O(V^2) \) worst case (requires exponential capacity
  ratios); \( O(V) \) in practice on non-pathological graphs.
* Total: \( O(V^2 (V+E) \log V) \) worst case;
  \( O(V (V+E) \log V) \) practical.

See also the Rust implementation @tide-maxflow@ which achieves \( O(VE) \)
practical complexity using O(1) array-based data structures.
 -}

{-# LANGUAGE BangPatterns #-}

module Data.Graph.AdjacencyList.PushRelabel.Pure
  ( -- * Main entry point
    pushRelabel
    -- * Algorithm internals (exported for testing)
  , tide
  , globalPush
  , globalPull
  , globalRelabel
  ) where

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

-- | Solve the maximum flow problem on a 'Network' using the Tide algorithm.
--
-- Returns @Right rg@ on success, where @rg@ is the 'ResidualGraph' at
-- termination.  The maximum flow value is @netFlow rg@ and per-edge flows
-- are available via @edgeFlow rg e@ or via @flow (network rg)@.
--
-- Returns @Left msg@ if an internal invariant is violated (should not happen
-- on valid inputs).
--
-- ==== Example
--
-- @
-- let g   = graphFromEdges [Edge 0 1, Edge 0 2, Edge 1 3, Edge 2 3]
--     caps = M.fromList [(Edge 0 1, 10), (Edge 0 2, 10), (Edge 1 3, 10), (Edge 2 3, 10)]
--     net  = Network g 0 3 caps (M.fromList [(e, 0) | e <- edges g])
-- case pushRelabel net of
--   Right rg -> print (netFlow rg)   -- 20
--   Left err -> putStrLn err
-- @
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

-- | Core recursive loop of the Tide algorithm.
--
-- Each call performs one tide: globalRelabel (unless skipped), then
-- globalPull, then globalPush.  Recurses until convergence (net flow
-- and overflowing set unchanged).
--
-- The @steps@ parameter counts completed iterations.
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

-- | Global push: sweep overflowing vertices from source to sink.
--
-- Iterates over overflowing vertices in /ascending level order/ (left fold
-- on the 'Overflowing' IntMap), pushing flow on all eligible /forward/
-- edges from each vertex.
--
-- This moves excess flow from source-side vertices towards the sink.
globalPush :: ResidualGraph -> ResidualGraph 
globalPush rg = 
  let ovfs = overflowing rg
   in IM.foldl' (\ac lset -> 
         Set.foldl' (\ac' v -> pushNeighbors ac' v)
         ac lset
      ) rg ovfs

-- | Global pull: sweep overflowing vertices from sink to source.
--
-- Iterates over overflowing vertices in /descending level order/ (right fold
-- on the 'Overflowing' IntMap), pulling flow on all eligible /reverse/
-- edges to each vertex.
--
-- This moves excess flow from sink-side vertices back towards the source.
globalPull :: ResidualGraph -> ResidualGraph
globalPull rg = 
  let ovfs = overflowing rg
   in IM.foldr' (\lset ac -> 
         Set.foldl' (\ac' v -> pullNeighbors ac' v)
         ac lset
               ) rg ovfs

-- | Push flow through all forward residual neighbors of a vertex.
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

-- | Pull flow through all reverse residual neighbors of a vertex.
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

-- | Global relabel: recompute vertex heights via BFS on the residual graph.
--
-- Runs BFS from both source and sink on the residual graph to compute
-- distances.  Sets vertex heights:
--
-- * Sink-side vertices: @height = distance_from_sink@
-- * Source-side vertices: @height = |V| + distance_from_source@
--
-- The height gap between source-side and sink-side vertices ensures
-- that flow can only move from source-side to sink-side (downhill).
globalRelabel :: ResidualGraph -> ResidualGraph
globalRelabel rg =
  let g = graph $ network rg
      sh = numVertices g
      s = source $ network rg
      t = sink $ network rg
      (slvs, tlvs) = residualDistances rg
      -- Vertices not reached by either BFS get height 2*|V| so their
      -- excess drains back to the source via pull operations.
      allVs = Set.fromList (vertices g)
      reachedS = Set.fromList (IM.keys slvs)
      reachedT = Set.fromList (IM.keys tlvs)
      reached = Set.union reachedS reachedT
      unreached = Set.difference allVs reached
      deadHeight = 2 * sh
      rg0 = Set.foldl' (\ac v -> updateHeight ac v deadHeight) rg unreached
      rg' = IM.foldrWithKey 
              (\ v l ac -> 
                 -- Heights for the source partition vertices is N + their distance to the source
                let h = sh + l 
                  in updateHeight ac v h
              ) rg0 slvs 
   in IM.foldrWithKey (\ v h ac
       -- Heights for the sink partition vertices equals the distance from sink
       -> updateHeight ac v h) 
       rg' tlvs
