{-|
Module      : Data.Graph.AdjacencyList.Metrics
Description : Graph distance and density metrics
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

Graph metrics computed from a 'Distances' matrix (see "Data.Graph.AdjacencyList.WFI"):
<https://en.wikipedia.org/wiki/Distance_(graph_theory) eccentricity>,
radius, diameter, and density.
 -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Graph.AdjacencyList.Metrics
  ( graphEccentricity
  , graphRadius
  , graphDiameter
  , graphDensity
  ) where

import Data.List
import Data.Maybe
import qualified Data.IntMap   as IM

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.WFI

-- | Eccentricity of a vertex: the maximum shortest-path distance from @v@
-- to any other reachable vertex.  Returns 'Nothing' if @v@ is not in the
-- distance matrix.
graphEccentricity :: Vertex -> Distances -> Maybe Weight
graphEccentricity v (Distances dis) =
  let vdis = IM.lookup v dis
   in maximum <$> vdis

-- | Radius of the graph: the minimum eccentricity over all vertices
-- (excluding zero and absent eccentricities).
graphRadius :: Distances -> Maybe Weight
graphRadius dis =
  let (Distances dism) = dis
      vs = IM.keys dism
      filtdis = filter (\d -> d /= Just 0 && d /= Nothing) 
         $ map (\v -> graphEccentricity v dis) vs
   in if null filtdis
         then Nothing
         else minimum filtdis

-- | Diameter of the graph: the maximum eccentricity over all vertices.
graphDiameter :: Distances -> Maybe Weight
graphDiameter dis =
  let (Distances dism) = dis
      vs = IM.keys dism
      filtdis = filter (\d -> d /= Just 0 && d /= Nothing) 
        $ map (\v -> graphEccentricity v dis) vs
   in if null filtdis
         then Nothing
         else maximum filtdis

-- | Since the representation of undirected graphs dublicated edges no need for
-- undirected version of density
graphDensity :: Graph -> Rational
graphDensity g =
  let ne = fromIntegral $ length $ edges g
      nv = fromIntegral $ length $ vertices g
   in ne / (nv * (nv - 1))
