{-|
Module      : Data.Graph.AdjacencyList.PushRelabel.Internal
Description : Residual graph types and primitive operations for the Tide algorithm
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Internal definitions for the Tide push-pull-relabel max-flow algorithm.

This module defines:

* 'ResidualGraph' — the mutable state threaded through each tide iteration,
  containing vertex heights, excesses, edge flows, and the set of overflowing
  vertices grouped by level.
* 'ResidualVertex' and 'ResidualEdge' — per-vertex and per-edge state.
* 'NeighborsMap' — an @IntMap@-based adjacency structure that maps each vertex
  to its forward and reverse neighbors with O(log V) edge-index lookup
  (replacing the original O(log E) @Map Edge Int@ lookup).
* Primitive operations: 'push', 'pull', 'updateHeight', 'updateExcess',
  'updateEdge', 'residualDistances'.

The 'topologyChanged' flag tracks whether any edge crossed a saturation
boundary (became saturated or unsaturated) during push\/pull.  When the
flag is 'False', the next tide can skip 'globalRelabel' — an optimization
that yields 1.25--1.61x speedup in practice.
 -}

{-# LANGUAGE BangPatterns #-}

module Data.Graph.AdjacencyList.PushRelabel.Internal
  ( -- * Re-exports from Network
    Network (..)
  , Capacity (..)
  , Capacities (..)
  , Flow 
    -- * Residual graph types
  , ResidualGraph (..)
  , ResidualVertex (..)
  , ResidualEdge (..)
  , NeighborsMap
  , Overflowing (..)
    -- * Vertex property types
  , Height
  , Excess
  , Level
    -- * Initialization
  , initializeResidualGraph
    -- * Vertex property accessors
  , level
  , excess
  , height
    -- * Edge property accessors
  , edgeCapacity
  , edgeFlow
  , resEdgeIndex
    -- * Flow queries
  , netFlow
  , inflow
  , outflow
  , sourceEdgesCapacity
    -- * Push and pull operations
  , push
  , pull
    -- * State updates
  , updateHeight
  , updateExcess
  , updateEdge
    -- * Overflowing vertex tracking
  , getOverflowing
    -- * Network reconstruction
  , networkFromResidual
    -- * Residual BFS (for globalRelabel)
  , residualDistances
    -- * Min-cut
  , stCut
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import qualified Data.Graph.AdjacencyList.BFS as BFS

-- | Vertex height in the push-relabel framework.
-- For source-side vertices: @height = |V| + distance_from_source@.
-- For sink-side vertices: @height = distance_from_sink@.
type Height = Int

-- | Vertex excess: @inflow - outflow@.  Positive excess means the vertex
-- is overflowing and needs to push or pull flow.
type Excess = Capacity

-- | Level: the shortest-path distance from the source in the /original/
-- (not residual) graph.  Constant throughout the algorithm.
-- Determines the ordering of vertices in globalPush (left fold, ascending)
-- and globalPull (right fold, descending).
type Level = Int

-- | Per-vertex state in the residual graph.
--
-- @ResidualVertex v l h x@ stores:
--
-- * @v@ — vertex identifier
-- * @l@ — level (BFS distance from source in original graph, constant)
-- * @h@ — height (updated by globalRelabel each tide)
-- * @x@ — excess flow (updated by push\/pull operations)
data ResidualVertex = ResidualVertex !Vertex !Level !Height !Excess
  deriving (Eq)
instance Show ResidualVertex where
  show (ResidualVertex v l h x) =
    "RVertex " ++ show v ++  " level: " ++
      show l ++ " height: " ++
      show h ++ " excess: " ++
      show (fromRational x :: Double)

type ResidualVertices = IM.IntMap ResidualVertex

-- | Per-edge state: original edge, capacity, and current flow (preflow).
--
-- @ResidualEdge e c f@: edge @e@ with capacity @c@ and flow @f@.
-- A forward residual edge exists when @f < c@; a backward residual edge
-- exists when @f > 0@.
data ResidualEdge = ResidualEdge Edge Capacity Flow
  deriving (Eq)
instance Show ResidualEdge where
  show (ResidualEdge e c f) =
    "REdge " ++ show e 
      ++  " " ++
      show (fromRational c :: Double)
      ++  " " ++
      show (fromRational f :: Double)
type ResidualEdges = IM.IntMap ResidualEdge

-- | For each vertex, maps forward neighbors and reverse neighbors
-- to their edge indices in the graph's 'EdgeMap'.
--
-- @NeighborsMap ! v = (fwdMap, revMap)@ where:
--
-- * @fwdMap ! w@ = index of edge @(v, w)@ (forward neighbor)
-- * @revMap ! u@ = index of edge @(u, v)@ (reverse neighbor)
--
-- This provides O(log degree) edge-index lookup, replacing the original
-- O(log E) lookup via @Map Edge Int@.
type NeighborsMap = IM.IntMap (IM.IntMap Int, IM.IntMap Int)

-- | Overflowing vertices grouped by level.
-- Keys are levels (BFS distance from source); values are sets of
-- vertices at that level with positive excess.
--
-- This structure determines the iteration order for globalPush
-- (ascending level = left fold) and globalPull (descending level = right fold).
type Overflowing = IM.IntMap Set.IntSet

-- | The residual graph: the complete mutable state of the Tide algorithm.
--
-- Threaded through each tide iteration.  Contains the underlying network,
-- per-vertex and per-edge state, the neighbor map for O(log V) edge lookup,
-- overflowing vertex sets, step counter, and the topology-change flag.
data ResidualGraph = 
  ResidualGraph { network :: !Network
                  -- ^ The original flow network.
                , netVertices :: !ResidualVertices
                  -- ^ Per-vertex state (level, height, excess).
                , netEdges :: !ResidualEdges 
                  -- ^ Per-edge state (capacity, flow).
                , netNeighborsMap :: !NeighborsMap 
                  -- ^ Adjacency map for O(log V) edge-index lookup.
                , overflowing :: !Overflowing
                  -- ^ Overflowing vertices grouped by level.
                , steps :: !Int
                  -- ^ Number of completed tide iterations.
                , topologyChanged :: !Bool
                  -- ^ Whether any edge crossed a saturation boundary
                  -- (became saturated or unsaturated) during the
                  -- most recent push\/pull phase.  When 'False',
                  -- the next tide can skip 'globalRelabel'.
                }
   deriving (Show,Eq)

-- | Build the initial 'ResidualGraph' from a 'Network'.
--
-- Saturates all edges leaving the source (setting their flow equal to
-- capacity), sets the source height to @|V|@, and initializes the
-- overflowing set with all vertices that received flow from the source.
--
-- The 'topologyChanged' flag is set to 'True' so the first tide always
-- runs 'globalRelabel'.
initializeResidualGraph :: Network -> ResidualGraph
initializeResidualGraph net = 
  let vs = initializeVertices net
      es = initializeEdges net
      neimap = getNetNeighborsMap $ graph net 
   in ResidualGraph { network = net
                    , netVertices = vs 
                    , netEdges = es 
                    , netNeighborsMap = neimap
                    , overflowing = 
                      let ovfs = getOverflowing vs
                          bfs = BFS.bfs (graph net) (source net)
                          maxLevel = BFS.maxLevel bfs
                          fl v = 
                            let (ResidualVertex _ l _ _) = 
                                  fromJust $ IM.lookup v vs
                             in l
                       in Set.foldl' 
                            (\ac v -> 
                               IM.adjust (\ps -> Set.insert v ps) (fl v) ac
                            ) (IM.fromList (zip [1..maxLevel] (repeat Set.empty))) ovfs
                    , steps = 0
                     , topologyChanged = True
                     } 

-- | Build the 'NeighborsMap' from a 'Graph'.
--
-- For each vertex @v@, computes:
--
-- * Forward map: @neighbor -> edgeIndex@ for edges @(v, neighbor)@
-- * Reverse map: @neighbor -> edgeIndex@ for edges @(neighbor, v)@
getNetNeighborsMap :: Graph -> NeighborsMap
getNetNeighborsMap g =
  let revgraph = reverseGraph g
      neis v = 
        let fwd = IM.fromList 
                    [ (n, fromJust $ edgeIndex g (Edge v n)) 
                    | n <- neighbors g v ]
            rev = IM.fromList 
                    [ (n, fromJust $ edgeIndex g (Edge n v)) 
                    | n <- neighbors revgraph v ]
         in (fwd, rev)
   in foldl' 
        (\ac v -> IM.insert v (neis v) ac) 
        IM.empty (vertices g)

-- | Look up forward and reverse neighbor maps for a vertex.
netNeighbors :: NeighborsMap 
             -> Vertex 
             -> (IM.IntMap Int, IM.IntMap Int) 
netNeighbors nm v = 
  fromJust $ IM.lookup v nm

-- | O(log degree) edge index lookup via 'NeighborsMap'.
--
-- Looks up the edge index of @(u, v)@ by finding @v@ in the forward
-- neighbor map of @u@.  Returns 'Nothing' if the edge does not exist.
resEdgeIndex :: NeighborsMap -> Edge -> Maybe Int
resEdgeIndex nm (Edge u v) = do
  (fwd, _) <- IM.lookup u nm
  IM.lookup v fwd

sourceEdges :: Network -> [(Edge,Capacity)]
sourceEdges net = 
  let g = graph net
      cs = capacities net
      s = source net
      cap v = fromJust $ M.lookup (Edge s v) cs
    in map (\v -> ((Edge s v), cap v )) (neighbors g s) 

-- | Total capacity of all edges leaving the source.
-- This is an upper bound on the maximum flow.
sourceEdgesCapacity :: Network -> Capacity
sourceEdgesCapacity net = 
  let ses = sourceEdges net
   in sum $ map snd ses

-- | Initialize vertex state: set source height to @|V|@, saturate source
-- edges (giving excess to source neighbors), set all other heights to 0.
initializeVertices :: Network -> ResidualVertices
initializeVertices net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      sh = fromIntegral $ numVertices g
      ses = sourceEdges net
      vs = vertices $ graph net
      flevels = BFS.level $ BFS.bfs (graph net) (source net)
      fl v = fromJust $ IM.lookup v flevels
      zvs = IM.fromList $ 
        zip (vertices g) (map (\v -> 
          ResidualVertex v (fl v) 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> 
        let v = to e
         in (cx-c, IM.adjust (const (ResidualVertex v (fl v) 0 c)) v ac)) (0, zvs) ses
   in IM.insert s (ResidualVertex s 0 sh sx) nvs

-- | Initialize edge state: saturate source edges, set all others to zero flow.
initializeEdges :: Network -> ResidualEdges
initializeEdges net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdge e c 0)) (M.toList cs)
      ses = sourceEdges net
   in  foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdge e c c) ac) inites ses 

-- | Collect all vertices with positive excess.
getOverflowing :: IM.IntMap ResidualVertex -> Set.IntSet
getOverflowing nvs = 
  let xv (ResidualVertex v _ _ x) = x
      vv (ResidualVertex v _ _ x) = v
   in Set.fromList $ map snd $ IM.toList (IM.map (\nv -> vv nv) (IM.filter (\nv -> xv nv > 0) nvs))

-- | Push flow along a /forward/ edge @(u, v)@.
--
-- Preconditions (checked, returns 'Nothing' if not met):
--
-- * @height(u) = height(v) + 1@ (flow goes downhill)
-- * Residual capacity @c - f > 0@ (edge is not saturated)
-- * @excess(u) > 0@ (source vertex has excess to push)
--
-- Pushes @min(excess(u), c - f)@ units of flow.
-- Updates the 'topologyChanged' flag if the edge becomes saturated.
push :: ResidualGraph -> Edge -> Maybe ResidualGraph
push g e =  
  let u = from e
      v = to e
      hu = height g u
      hv = height g v 
      xu = excess g u 
      xv = excess g v
      c = edgeCapacity g e
      f = edgeFlow g e
      nvs = netVertices g
      xf = min xu (c - f)
   in if (hu == hv + 1) && xf > 0
         then
           let g' = foldr (\f ac -> f ac) g
                      [ (\nt -> updateEdge nt e (f + xf))
                      , (\nt -> updateExcess nt u (xu - xf))
                      , (\nt -> updateExcess nt v (xv + xf))
                      ]
            in Just g'
         else Nothing 

-- | Pull flow along a /reverse/ edge @(u, v)@.
--
-- This is the dual of 'push': it decreases flow on edge @(u, v)@ by moving
-- excess from @v@ back to @u@.
--
-- Preconditions (checked, returns 'Nothing' if not met):
--
-- * @height(v) = height(u) + 1@ (pull goes uphill in the forward direction)
-- * @flow(u, v) > 0@ (there is flow to pull back)
-- * @excess(v) > 0@ (pulling vertex has excess)
--
-- Pulls @min(excess(v), flow)@ units.
-- Updates the 'topologyChanged' flag if the edge becomes zero-flow.
pull :: ResidualGraph -> Edge -> Maybe ResidualGraph
pull g e  = 
  let u   = from e
      v   = to e
      hu  = height g u
      hv  = height g v 
      xu  = excess g u 
      xv  = excess g v
      c   = edgeCapacity g e
      f   = edgeFlow g e
      nvs = netVertices g
      xf  = min xv f
   in if (hv == hu + 1) && xf > 0 
         then
           let g' = foldr (\f ac -> f ac) g
                     [ (\nt -> updateEdge nt e (f - xf))
                     , (\nt -> updateExcess nt u (xu + xf))
                     , (\nt -> updateExcess nt v (xv - xf))
                     ]
            in Just g'
         else Nothing 

-- | Update the height of a vertex.  Source and sink heights are never modified.
updateHeight :: ResidualGraph -> Vertex -> Height -> ResidualGraph
updateHeight g v nh =
  let netvs = netVertices g
      !nv = fromJust $ IM.lookup v netvs
      !x = excess g v
      !l = level g v
      !s = source $ network g
      !t = sink $ network g
      !nnetv = IM.update (\_ -> Just (ResidualVertex v l nh x)) v netvs
  in if v == t || v == s 
        then g
        else g { netVertices = nnetv }

-- | Update the excess of a vertex and maintain the 'overflowing' index.
--
-- When excess transitions between zero and non-zero, the vertex is
-- added to or removed from the 'Overflowing' map at its level.
-- Source and sink are excluded from the overflowing set.
updateExcess :: ResidualGraph -> Vertex -> Excess -> ResidualGraph
updateExcess g v nx =
  let netvs = netVertices g
      nv = fromJust $ IM.lookup v netvs
      h = height g v
      l = level g v
      ovfs = overflowing g
      s = source $ network g
      t = sink $ network g
      newovfs = 
        if v == s || v == t
           then ovfs
           else
             let ovfs' = IM.update (\lvs -> 
                         let lset = Set.delete v lvs
                          in if Set.null lset
                                      then Nothing 
                                      else Just lset) l ovfs
              in if nx == 0
                then 
                  ovfs'
                else 
                  let mlset = IM.lookup l ovfs'
                   in case mlset of 
                        Nothing -> IM.insert l (Set.singleton v) ovfs'
                        Just lset -> IM.adjust (Set.insert v) l ovfs'
   in if v == t then g
                else g { netVertices = IM.insert v (ResidualVertex v l h nx) netvs
                       , overflowing = newovfs
                       } 

-- | Update the flow on an edge and track topology changes.
--
-- A topology change occurs when a forward residual edge appears or
-- disappears (flow crosses the capacity boundary) or a backward residual
-- edge appears or disappears (flow crosses zero).
-- The 'topologyChanged' flag is set to 'True' (OR-ed) if such a change occurs.
updateEdge :: ResidualGraph -> Edge -> Flow -> ResidualGraph
updateEdge g e f =
  let es = netEdges g
      eid = fromJust $ resEdgeIndex (netNeighborsMap g) e
      (ResidualEdge e' c f') = fromJust $ IM.lookup eid es
      -- Detect if edge crossed a saturation boundary:
      -- forward edge exists iff flow < capacity
      -- backward edge exists iff flow > 0
      !fwdBefore = f' < c
      !fwdAfter  = f < c
      !bwdBefore = f' > 0
      !bwdAfter  = f > 0
      !changed   = (fwdBefore /= fwdAfter) || (bwdBefore /= bwdAfter)
   in g { netEdges = IM.adjust (const (ResidualEdge e c f)) eid es
        , topologyChanged = topologyChanged g || changed
        }

-- | Net flow into the sink.  This is the current flow value of the network.
-- At termination, this equals the maximum flow.
netFlow :: ResidualGraph -> Flow
netFlow g = inflow g (sink (network g))

-- | Height of a vertex.
height :: ResidualGraph -> Vertex -> Height
height rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in h

-- | Excess of a vertex.
excess :: ResidualGraph -> Vertex -> Excess
excess rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in x

-- | Level of a vertex (shortest distance from source in original graph).
level :: ResidualGraph -> Vertex -> Level
level rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in l

-- | Capacity of an edge.
edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ resEdgeIndex (netNeighborsMap g) e) (netEdges g)
                    in c 

-- | Current flow on an edge.
edgeFlow :: ResidualGraph -> Edge -> Flow
edgeFlow g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ resEdgeIndex (netNeighborsMap g) e) (netEdges g)
                in f 

-- | Total flow into a vertex (sum of flows on incoming edges).
inflow :: ResidualGraph -> Vertex -> Flow
inflow g v =
  let (_, revMap) = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (n,v)) $ IM.keys revMap
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 

-- | Total flow out of a vertex (sum of flows on outgoing edges).
outflow :: ResidualGraph -> Vertex -> Flow
outflow g v =
  let (fwdMap, _) = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (v,n)) $ IM.keys fwdMap
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 

-- | Reconstruct the 'Network' with final edge flows from the residual graph.
-- Called when the algorithm terminates.
networkFromResidual :: ResidualGraph -> Network
networkFromResidual resg =
  let net = network resg
      es = edges $ graph $ net
      flow' = M.fromList $ map (\e -> (e, edgeFlow resg e) ) es
   in net {flow = flow'}

-- | Compute distances from source and sink in the residual graph via BFS.
--
-- Returns @(sourceDists, sinkDists)@ where:
--
-- * @sourceDists@: @IntMap@ from vertex to BFS distance from source
--   (traversing edges with residual capacity > 0 in reverse, and edges
--   with flow > 0 forward)
-- * @sinkDists@: @IntMap@ from vertex to BFS distance from sink
--   (traversing edges with residual capacity > 0 forward, and edges
--   with flow > 0 in reverse)
--
-- Used by 'globalRelabel' to set vertex heights:
-- source-side vertices get @height = |V| + dist_from_source@,
-- sink-side vertices get @height = dist_from_sink@.
residualDistances :: ResidualGraph -> (IM.IntMap Int, IM.IntMap Int)
residualDistances rg = 
  let es = map snd (IM.toList $ netEdges rg)
      -- forward residual edges (flow < capacity)
      tres = filter (\(ResidualEdge e c f) -> f < c) es
      -- backward residual edges (flow > 0)
      tbes = filter (\(ResidualEdge e c f) -> f > 0) es
      tfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert v [u] ac
               Just ns -> IM.insert v (u:ns) ac
             ) IM.empty tres
      tsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup u ac 
         in case mns of 
               Nothing -> IM.insert u [v] ac
               Just ns -> IM.insert u (v:ns) ac
             ) tfsatnbs tbes
      sfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup u ac 
         in case mns of 
               Nothing -> IM.insert u [v] ac
               Just ns -> IM.insert u (v:ns) ac
             ) IM.empty tres
      ssatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert v [u] ac
               Just ns -> IM.insert v (u:ns) ac
             ) sfsatnbs tbes
      tlvs = BFS.level $ BFS.adjBFS tsatnbs t
      slvs = BFS.level $ BFS.adjBFS ssatnbs s
    in (slvs, tlvs)
  where
    g = graph $ network rg
    s = source $ network rg
    t = sink $ network rg

-- | Compute the source-sink minimum cut from the residual graph.
--
-- Returns @(S, T)@ where @S@ is the set of vertices reachable from the
-- source in the residual graph (excluding source and sink) and @T@ is
-- the complement.  By the max-flow min-cut theorem, the total capacity
-- of edges crossing from @S@ to @T@ equals the maximum flow.
stCut :: ResidualGraph -> ([Vertex],[Vertex])
stCut rg = 
  let !resdis = residualDistances rg
      ts = Set.delete s $ Set.delete t $ Set.fromList $ map fst (IM.toList (snd resdis))
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      vs = Set.delete s $ Set.delete t $ Set.fromList $ vertices g
      ss = Set.difference vs ts
   in (Set.toList ss, Set.toList ts)
