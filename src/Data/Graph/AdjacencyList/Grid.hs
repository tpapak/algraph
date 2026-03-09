{-|
Module      : Data.Graph.AdjacencyList.Grid
Description : d-dimensional cubic lattices with periodic boundary conditions
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX

Generators for d-dimensional cubic lattices with periodic boundary conditions
(toroidal topology).  A 'PBCSquareLattice' @L D@ is the Cartesian product of
@D@ cycle graphs of length @L@: \( C_L \square^D \).

Provides both directed ('graphCubicPBC', forward edges only) and undirected
('undirectedGraphCubicPBC', forward + backward edges) variants, plus coordinate
conversion between flat vertex IDs and Cartesian coordinates.
 -}

module Data.Graph.AdjacencyList.Grid
    ( L
    , D
    , CVertex
    , fromTuple
    , toTuple
    , adjacentEdges
    , vertexToCVertex
    , cVertexToVertex
    , PBCSquareLattice (..)
    , pbcEdgeIx
    , gridSize
    , gridNumEdges
    , pbcForwardEdges
    , pbcBackwardEdges
-- * Undirected cubic graph with PBC
    , undirectedGraphCubicPBC
-- * Directed cubic graph with PBC
    , graphCubicPBC
    ) where

import Data.List
import qualified Data.Map.Lazy as M
import Numeric.Natural

import Data.Graph.AdjacencyList

-- | Linear size of the lattice (number of vertices per dimension).
type L = Natural

-- | Dimensionality of the lattice (2 = square, 3 = cubic, etc.).
type D = Natural

-- | Cartesian coordinates of a lattice vertex: a list of per-dimension indices.
type CVertex = [Vertex]
data CEdge = CEdge CVertex CVertex -- ^ Cartesian representation of a Lattice Vertex

data Direction = Forward | Backward deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | A PBCSquareLattice is the Cartesian product of a cycle graph of length L
-- (C_L) => (C_L)▢^d
data PBCSquareLattice = PBCSquareLattice L D
instance Eq PBCSquareLattice where 
  (==) (PBCSquareLattice la da) (PBCSquareLattice lb db) = 
    la == la && da == db        
instance Show PBCSquareLattice where 
  show (PBCSquareLattice l d) = "Lattice: { \n" ++
                  " L : " ++ show l ++ "\n" ++
                  " D : " ++ show d ++ "\n" ++
                    " numVertices : " ++ show (gridN l d) ++ "\n" ++
                      " numEdges : " ++ show (gridNumEdges (PBCSquareLattice l d))

-- | Undirected graph on a PBC cubic lattice (both forward and backward edges).
-- Contains @2 * D * L^D@ directed edges (two per neighbor pair).
undirectedGraphCubicPBC :: PBCSquareLattice -> Graph
undirectedGraphCubicPBC (PBCSquareLattice l d) = 
  let vs = gridVertices l d
      neis = pbcUndirectedNeighbors l d
   in createGraph vs neis

-- | Directed graph embeded in cubic lattice
graphCubicPBC :: PBCSquareLattice -> Graph
graphCubicPBC (PBCSquareLattice l d) = 
  let vs = gridVertices l d
      neis = pbcDirectedNeighbors l d
   in createGraph vs neis

-- | Number of directed (forward) edges in the lattice: @D * L^D@.
gridNumEdges :: PBCSquareLattice -> Natural
gridNumEdges (PBCSquareLattice l d) = d * (gridN l d)

gridN :: L -> D -> Natural
gridN l d = l ^ d

-- | Total number of vertices in the lattice: @L^D@.
gridSize :: PBCSquareLattice -> Natural
gridSize (PBCSquareLattice l d) =  gridN l d

gridVertices :: L -> D -> [Vertex]
gridVertices l d = [1 .. (fromEnum l ^ fromEnum d)]

-- | Returns the next vertex of v in the d dimension for a grid of side l
pbcNeighbor :: Vertex -> L -> D -> Direction -> Vertex
pbcNeighbor v l d r 
  | r == Forward =
    if not $! isBoundary v l d
      then v + innerOffset l d
      else v + pbcOffset l d 
  | r == Backward =
    if not $ isBoundary (v - innerOffset l d) l d
      then v - innerOffset l d
      else v - pbcOffset l d
  where
    l' = fromEnum l
    d' = fromEnum d
    innerOffset :: L -> D -> Vertex
    innerOffset l d = l'^(d' - 1)
    pbcOffset :: L -> D -> Vertex
    pbcOffset l d = - l'^d + l'^(d' - 1)
    isBoundary :: Vertex -> L -> D -> Bool
    isBoundary v l d = (l'^d') - (l'^(d' - 1)) - mod (v - 1) (l'^d') <= 0

-- | Given vertex returns list of nearest neighboring vertices on a Toroidal Boundary Conditions (pbc) grid
pbcDirectedNeighbors :: L -> D -> Neighbors
pbcDirectedNeighbors l d v = fmap (\d'-> pbcNeighbor v l d' Forward) [1 .. d]

-- | Given vertex returns list of nearest neighboring vertices on a Toroidal Boundary Conditions (pbc) grid
pbcUndirectedNeighbors :: L -> D -> Vertex -> [Vertex]
pbcUndirectedNeighbors l d v = (\r d'-> pbcNeighbor v l d' r) 
  <$> [Forward,Backward] <*> [1 .. d]

-- | Given a Vertex returns a tuple of the Cartesian product of a L sized Cycle graph
vertexToCVertex :: L -> D -> Vertex -> CVertex
vertexToCVertex l' d' v = do
  let cix l n i = (mod (div (n-1) (l^(i-1))) l) + 1
      out = map (cix l v) [1 .. d]
  out
  where l = fromEnum l'
        d = fromEnum d'

-- | The reverse function of vertexToCVertex
cVertexToVertex :: L -> D -> CVertex -> Vertex
cVertexToVertex l' d' cv = do
  (foldr (\t@(i,x)-> (+) ((x-1) * (l^(i-1)))) 0 $ zip [1 .. d] cv) + 1
  where l = fromEnum l'
        d = fromEnum d'

-- | Gives Forward vertex in a cycle graph of length L
forwardVertexInCycle :: L -> Vertex -> Vertex
forwardVertexInCycle l' v
  | v == l = 1
  | otherwise = v + 1
  where l = fromEnum l'

-- | Gives Forward vertex in a cycle graph of length L
backwardVertexInCycle :: L -> Vertex -> Vertex
backwardVertexInCycle l' v
  | v == 1 = l
  | otherwise = v - 1
  where l = fromEnum l'

-- | Given two edges returns if they belong to the lattice
isEdgeInCycle :: L -> Edge -> Bool
isEdgeInCycle l' (Edge a b)
  | a == b - 1 = True
  | a == b + 1 = True
  | a == l && b == 1 = True
  | b == l && a == 1 = True
  | otherwise = False
  where l = fromEnum l'

-- | Returns tuple (edge) giving forward vertices of given vertex on a Toroidal Boundary Conditions (pbc) grid
pbcForwardEdges :: L -> D -> Vertex -> [Edge]
pbcForwardEdges l d v = fmap (\d -> Edge v (pbcNeighbor v l d Forward)) [1 .. d]

-- | Returns tuple (edge) giving backward vertices of given vertex on a Toroidal Boundary Conditions (pbc) grid
pbcBackwardEdges :: L -> D -> Vertex -> [Edge]
pbcBackwardEdges l d v = fmap (\d -> Edge v (pbcNeighbor v l d Backward)) [1 .. d]

pbcUndirectedEdges :: L -> D -> [Edge]
pbcUndirectedEdges l d = 
  let nei v = 
        foldl' 
          (\ac d -> ac ++
              [ Edge v (pbcNeighbor v l d Forward)
              , Edge v (pbcNeighbor v l d Backward)
              ]
          )[] [1 .. d]
   in foldr (\v ac -> (nei v) ++ ac) [] $ gridVertices l d

-- | Returns tuple (edge) giving forward and backward vertices of given vertex on a Toroidal Boundary Conditions (pbc) grid
pbcAdjacentEdges :: L -> D -> Vertex -> [Edge]
pbcAdjacentEdges l d v = (\r d -> 
  case r of Forward ->  Edge v (pbcNeighbor v l d r)
            Backward -> Edge (pbcNeighbor v l d r) v
  ) 
  <$> [Forward,Backward] <*> [1 .. d]

-- | List of edges of grid with periodic boundary conditions
pbcDirectedEdges :: L -> D -> [Edge]
pbcDirectedEdges l d = (\v j-> Edge v (pbcNeighbor v l j Forward)) <$> gridVertices l d <*> [1 .. d]

-- | Index of edge of a grid with periodic boundary conditions
-- Very inefficient, better use Data.Map for lookups.
pbcEdgeIx :: L -> D -> Edge -> Maybe Int
pbcEdgeIx l d e = do
  let Edge s t = e
      a = vertexToCVertex l d s
      b = vertexToCVertex l d t
      (((a',b'),di),dist) = diff (CEdge a b)
  case dist == 1 of
    True -> case forwardVertexInCycle l a' == b' of
              True -> Just $ ((s-1)*d') + di
              False -> Just $ ((t-1)*d') + di
    False -> Nothing
  where
    d' = fromEnum d
    step (((a',b'),di'), ds) ((s,t),di)
      | s == t = (((a',b'),di'), ds)
      | s /= t = (((s,t),di), ds+1)
    diff :: CEdge -> (((Vertex,Vertex),Int),Int)
    diff (CEdge a b) = foldl step (((0,0),0),0) $ zip (zip a b) [1..d']

