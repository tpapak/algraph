{-|
Module      : PushRelabel - PureOld
Description : Old implementation using M.Map Edge Int lookups (edgeIndex).
              Kept temporarily to verify the new IntMap-based implementation
              produces identical results. To be removed in next commit.
-}

{-# LANGUAGE BangPatterns #-}

module Data.Graph.AdjacencyList.PushRelabel.PureOld
  ( pushRelabelOld
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Internal

-- ================================================================
-- Edge operations using the OLD edgeIndex (M.Map Edge Int, O(log E))
-- ================================================================

edgeCapacityOld :: ResidualGraph -> Edge -> Capacity
edgeCapacityOld g e =
  let (ResidualEdge _ c _) =
        fromJust $ IM.lookup
          (fromJust $ edgeIndex (graph $ network g) e)
          (netEdges g)
   in c

edgeFlowOld :: ResidualGraph -> Edge -> Flow
edgeFlowOld g e =
  let (ResidualEdge _ _ f) =
        fromJust $ IM.lookup
          (fromJust $ edgeIndex (graph $ network g) e)
          (netEdges g)
   in f

updateEdgeOld :: ResidualGraph -> Edge -> Flow -> ResidualGraph
updateEdgeOld g e f =
  let es  = netEdges g
      eid = fromJust $ edgeIndex (graph $ network g) e
      (ResidualEdge e' c f') = fromJust $ IM.lookup eid es
   in g { netEdges = IM.adjust (const (ResidualEdge e c f)) eid es }

-- ================================================================
-- Push / Pull using old edge lookups
-- ================================================================

pushOld :: ResidualGraph -> Edge -> Maybe ResidualGraph
pushOld g e =
  let u  = from e
      v  = to e
      hu = height g u
      hv = height g v
      xu = excess g u
      xv = excess g v
      c  = edgeCapacityOld g e
      f  = edgeFlowOld g e
      xf = min xu (c - f)
   in if (hu == hv + 1) && xf > 0
        then
          let g' = foldr (\fn ac -> fn ac) g
                     [ (\nt -> updateEdgeOld nt e (f + xf))
                     , (\nt -> updateExcess nt u (xu - xf))
                     , (\nt -> updateExcess nt v (xv + xf))
                     ]
           in Just g'
        else Nothing

pullOld :: ResidualGraph -> Edge -> Maybe ResidualGraph
pullOld g e =
  let u  = from e
      v  = to e
      hu = height g u
      hv = height g v
      xu = excess g u
      xv = excess g v
      c  = edgeCapacityOld g e
      f  = edgeFlowOld g e
      xf = min xv f
   in if (hv == hu + 1) && xf > 0
        then
          let g' = foldr (\fn ac -> fn ac) g
                     [ (\nt -> updateEdgeOld nt e (f - xf))
                     , (\nt -> updateExcess nt u (xu + xf))
                     , (\nt -> updateExcess nt v (xv - xf))
                     ]
           in Just g'
        else Nothing

-- ================================================================
-- Neighbor traversal
-- ================================================================

pushNeighborsOld :: ResidualGraph -> Vertex -> ResidualGraph
pushNeighborsOld g v =
  let neimap = netNeighborsMap g
      (fwdMap, _) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) $ IM.keys fwdMap
   in foldl' (\ac e ->
        case pushOld ac e of
          Nothing  -> ac
          Just g'' -> g'') g feds

pullNeighborsOld :: ResidualGraph -> Vertex -> ResidualGraph
pullNeighborsOld g v =
  let neimap = netNeighborsMap g
      (_, revMap) = fromJust $ IM.lookup v neimap
      reds = map (\n -> fromTuple (n,v)) $ IM.keys revMap
   in foldl' (\ac e ->
        case pullOld ac e of
          Nothing  -> ac
          Just g'' -> g'') g reds

-- ================================================================
-- Global operations
-- ================================================================

globalPushOld :: ResidualGraph -> ResidualGraph
globalPushOld rg =
  let ovfs = overflowing rg
   in IM.foldl' (\ac lset ->
        Set.foldl' (\ac' v -> pushNeighborsOld ac' v) ac lset
      ) rg ovfs

globalPullOld :: ResidualGraph -> ResidualGraph
globalPullOld rg =
  let ovfs = overflowing rg
   in IM.foldr' (\lset ac ->
        Set.foldl' (\ac' v -> pullNeighborsOld ac' v) ac lset
      ) rg ovfs

globalRelabelOld :: ResidualGraph -> ResidualGraph
globalRelabelOld rg =
  let g   = graph $ network rg
      sh  = numVertices g
      (slvs, tlvs) = residualDistances rg
      rg' = IM.foldrWithKey
              (\v l ac -> let h = sh + l in updateHeight ac v h)
              rg slvs
   in IM.foldrWithKey (\v h ac -> updateHeight ac v h) rg' tlvs

-- ================================================================
-- Tide + pushRelabel (old)
-- ================================================================

tideOld :: ResidualGraph -> Int -> ResidualGraph
tideOld rg stps =
  let g      = rg `seq` (graph $ network rg)
      s      = source $ network rg
      t      = sink $ network rg
      es     = edges g
      vs     = vertices g
      olf    = netFlow rg
      bfsrg  = globalRelabelOld rg
      rg'    = globalPushOld $ globalPullOld bfsrg
      nfl    = netFlow rg'
      stps'  = stps + 1
      oovfls = overflowing rg
      novfls = overflowing rg'
   in if nfl == olf
        then if oovfls == novfls
               then rg' { network = networkFromResidual rg'
                        , steps   = stps' }
               else tideOld rg' stps'
        else tideOld rg' stps'

pushRelabelOld :: Network -> Either String ResidualGraph
pushRelabelOld net =
  let initg   = initializeResidualGraph net
      res     = tideOld initg 0
      nvs     = vertices $ graph $ network res
      s       = source net
      t       = sink net
      insouts = filter (\v -> v /= s && v /= t
                           && inflow res v < outflow res v) nvs
      xsflows = filter (\v -> v /= s && v /= t
                           && inflow res v - outflow res v /= excess res v) nvs
      ofvs    = IM.foldr (\ovs ac -> Set.union ac ovs) Set.empty
                  $ overflowing res
      notofvs = filter (\ov ->
                  let (ResidualVertex v l h x) =
                        fromJust (IM.lookup ov (netVertices res))
                      ml = IM.lookup l (overflowing res)
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
            else if not $ null xsflows
              then Left $ "Error vertex excess " ++ show xsflows
              else if not $ Set.null errovfs
                then Left $ "Error not really overflowing " ++ show errovfs
                else Left $ "Error not in overflowing " ++ show notofvs
                  ++ " overflowings are " ++ show (overflowing res)
                  ++ " nevertices are " ++ show (netVertices res)
