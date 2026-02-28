{-|
Module      : Data.Graph.AdjacencyList.Network
Description : Flow network data type for max-flow problems
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Defines the 'Network' type used as input to the Tide max-flow algorithm.

A 'Network' consists of:

* A directed 'Graph'
* A distinguished 'source' and 'sink' vertex
* Edge 'Capacities' (mapping each edge to a non-negative 'Rational')
* Edge flows (initially zero, filled in by the solver)

Capacities use 'Rational' for exact arithmetic — the Tide algorithm
terminates correctly for arbitrary rational capacities.  For integer-only
workloads, see the Rust implementation @tide-maxflow@ which uses @i64@.
 -}

module Data.Graph.AdjacencyList.Network
  ( -- * Network type
    Network (..)
    -- * Type aliases
  , Capacity
  , Capacities
  , Flow
    -- * Utilities
  , uniformCapacities
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntSet as Set

import Data.Graph.AdjacencyList

-- | Edge capacity.  Uses 'Rational' for exact arithmetic, ensuring the
-- Tide algorithm terminates correctly for arbitrary capacity values.
type Capacity = Rational 

-- | Map from edges to their capacities.
type Capacities = M.Map Edge Capacity 

-- | Edge flow.  Same type as 'Capacity' since flow values are rational.
type Flow = Capacity

showCapacities :: Capacities -> String
showCapacities cps =
  show $ fmap (\c -> fromRational c :: Double) cps

-- | A flow network: a directed graph with a source, sink, edge capacities,
-- and edge flows.
--
-- Construct a 'Network' with zero initial flows and pass it to
-- 'Data.Graph.AdjacencyList.PushRelabel.Pure.pushRelabel' to compute the
-- maximum flow.
data Network = Network { graph :: !Graph
                         -- ^ The underlying directed graph.
                       , source :: Vertex
                         -- ^ Source vertex (flow originates here).
                       , sink :: Vertex
                         -- ^ Sink vertex (flow terminates here).
                       , capacities :: Capacities
                         -- ^ Edge capacities.  Every edge in 'graph' must
                         -- have a corresponding entry.
                       , flow :: Capacities
                         -- ^ Edge flows.  Set to zero initially; filled in
                         -- by the solver.
                       }
                       deriving (Eq)

instance Show Network where
  show net =
    "Network" <> show (graph net) <> "\n"
    <> " source: " <> show (source net) <> "\n"
    <> " sink  : " <> show (sink net) <> "\n"
    <> " capacities: " <> showCapacities (capacities net) <> "\n"
    <> " flows: " <> showCapacities (flow net) <> "\n"

-- | Set all edge capacities to 1 (unit capacity network).
uniformCapacities :: Graph -> Capacities
uniformCapacities g =
  M.fromList $ map (\e -> (e,1)) $ edges g
