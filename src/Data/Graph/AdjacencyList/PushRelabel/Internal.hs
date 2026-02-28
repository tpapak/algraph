{-|
Module      : PushRelabel - Internal
Description : Maximum Flow - Min Cut - Push relabel algorithm - Definitions
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

 -}

{-# LANGUAGE BangPatterns #-}

module Data.Graph.AdjacencyList.PushRelabel.Internal
  ( Network (..)
  , ResidualGraph (..)
  , NeighborsMap
  , ResidualVertex (..)
  , ResidualEdge (..)
  , Overflowing (..)
  , Capacity (..)
  , Capacities (..)
  , Flow 
  , Height
  , Excess
  , Level -- * Shortest distance from source
  , initializeResidualGraph
  , level
  , excess
  , height
  , netFlow
  , push
  , pull
  , getOverflowing
  , inflow
  , outflow
  , networkFromResidual
  , updateHeight
  , updateExcess
  , updateEdge
  , resEdgeIndex
  , edgeCapacity
  , edgeFlow
  , sourceEdgesCapacity
  , residualDistances
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

type Height = Int
type Excess = Capacity
type Level = Int -- ^ Shortest distance from source

data ResidualVertex = ResidualVertex !Vertex !Level !Height !Excess
  deriving (Eq)
instance Show ResidualVertex where
  show (ResidualVertex v l h x) =
    "RVertex " ++ show v ++  " level: " ++
      show l ++ " height: " ++
      show h ++ " excess: " ++
      show (fromRational x :: Double)

type ResidualVertices = IM.IntMap ResidualVertex

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
-- to their edge indices in the graph's EdgeMap.
-- Forward: neighbor -> edgeIndex of (v, neighbor)
-- Reverse: neighbor -> edgeIndex of (neighbor, v)
type NeighborsMap = IM.IntMap (IM.IntMap Int, IM.IntMap Int)

-- | Keys are the level (shortest distance from source) and the value is the set
-- of overflowing vertices
type Overflowing = IM.IntMap Set.IntSet

data ResidualGraph = 
  ResidualGraph { network :: !Network
                , netVertices :: !ResidualVertices
                , netEdges :: !ResidualEdges 
                , netNeighborsMap :: !NeighborsMap 
                , overflowing :: !Overflowing -- ^ Set of overflowing vertices
                , steps :: !Int
                , topologyChanged :: !Bool -- ^ Whether any edge crossed a saturation boundary
                }
   deriving (Show,Eq)

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
                     , topologyChanged = True -- ^ First tide must always run globalRelabel
                     } 

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

-- | graph and reverse (inward and outward) neighbors
netNeighbors :: NeighborsMap 
             -> Vertex 
             -> (IM.IntMap Int, IM.IntMap Int) 
netNeighbors nm v = 
  fromJust $ IM.lookup v nm

-- | O(log degree) edge index lookup via NeighborsMap
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

sourceEdgesCapacity :: Network -> Capacity
sourceEdgesCapacity net = 
  let ses = sourceEdges net
   in sum $ map snd ses

-- | Set adjacent to source vertices heigts to \(N\) and their excesses accordingly
-- given that initial source edges will be saturated
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

-- | Saturate source edges with flow the other ones set flow to 0
initializeEdges :: Network -> ResidualEdges
initializeEdges net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdge e c 0)) (M.toList cs)
      ses = sourceEdges net
   in  foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdge e c c) ac) inites ses 

getOverflowing :: IM.IntMap ResidualVertex -> Set.IntSet
getOverflowing nvs = 
  let xv (ResidualVertex v _ _ x) = x
      vv (ResidualVertex v _ _ x) = v
   in Set.fromList $ map snd $ IM.toList (IM.map (\nv -> vv nv) (IM.filter (\nv -> xv nv > 0) nvs))

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

netFlow :: ResidualGraph -> Flow
netFlow g = inflow g (sink (network g))

height :: ResidualGraph -> Vertex -> Height
height rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in h

excess :: ResidualGraph -> Vertex -> Excess
excess rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in x

level :: ResidualGraph -> Vertex -> Level
level rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in l

edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ resEdgeIndex (netNeighborsMap g) e) (netEdges g)
                    in c 

edgeFlow :: ResidualGraph -> Edge -> Flow
edgeFlow g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ resEdgeIndex (netNeighborsMap g) e) (netEdges g)
                in f 

inflow :: ResidualGraph -> Vertex -> Flow
inflow g v =
  let (_, revMap) = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (n,v)) $ IM.keys revMap
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 

outflow :: ResidualGraph -> Vertex -> Flow
outflow g v =
  let (fwdMap, _) = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (v,n)) $ IM.keys fwdMap
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 

-- | Update flow of network from the residual edges' preflow
networkFromResidual :: ResidualGraph -> Network
networkFromResidual resg =
  let net = network resg
      es = edges $ graph $ net
      flow' = M.fromList $ map (\e -> (e, edgeFlow resg e) ) es
   in net {flow = flow'}

-- | Partitions residual graph into source sink cut and stores the distance from
-- source and sink respectively:
-- (distance from source (only those that don't connect
-- to sink), distance from sink)
residualDistances :: ResidualGraph -> (IM.IntMap Int, IM.IntMap Int)
residualDistances rg = 
  let es = map snd (IM.toList $ netEdges rg)
      -- | forward residual edges
      tres = filter (\(ResidualEdge e c f) -> f < c) es
      -- | backward residual edges
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
            mns = IM.lookup v ac 
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

-- | source - sink partition 
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
