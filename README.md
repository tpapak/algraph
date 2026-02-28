
# graph — Pure Haskell Graph Library

`Data.Graph.AdjacencyList` — a pure Haskell graph library using adjacency list representation,
featuring the **Tide algorithm**, a push-pull-relabel max-flow solver.

## Modules

### Core

| Module | Description |
|--------|-------------|
| `Data.Graph.AdjacencyList` | Graph type (`Vertex`, `Edge`, `Neighbors`, `EdgeMap`), constructors (`graphFromEdges`, `createGraph`), utilities (`reverseGraph`, `makeUndirected`, `completeGraph`, `edgeIndex`, `adjacencyMap`) |
| `Data.Graph.AdjacencyList.Network` | Flow network type (`Network`, `Capacities`, `Capacity = Rational`), input to the Tide algorithm |
| `Data.Graph.AdjacencyList.Grid` | d-dimensional cubic lattices with periodic boundary conditions (PBC). `PBCSquareLattice L D` — Cartesian product of cycle graphs C_L^d. Directed and undirected variants. Coordinate conversion utilities |

### Algorithms

| Module | Description |
|--------|-------------|
| `Data.Graph.AdjacencyList.PushRelabel.Pure` | **Tide algorithm** — push-pull-relabel max-flow solver. Each iteration (tide) performs three global sweeps: `globalRelabel` (BFS on residual graph), `globalPull` (reverse-edge flow), `globalPush` (forward-edge flow). Includes skip-globalRelabel optimization. Complexity: O(V²(V+E) log V) worst case, O(V(V+E) log V) practical |
| `Data.Graph.AdjacencyList.PushRelabel.Internal` | Residual graph data structures, push/pull primitives, `NeighborsMap` with O(log V) edge-index lookup, `residualDistances`, s-t cut extraction |
| `Data.Graph.AdjacencyList.BFS` | Breadth-first search with level/distance map, parent map, and spanning tree extraction |
| `Data.Graph.AdjacencyList.DFS` | Depth-first search with topological sort, longest path, connectivity queries |
| `Data.Graph.AdjacencyList.WFI` | Floyd-Warshall all-pairs shortest paths |
| `Data.Graph.AdjacencyList.Metrics` | Graph eccentricity, radius, diameter, density |

## The Tide Algorithm

The Tide algorithm is a level-synchronous push-pull-relabel max-flow algorithm.
Unlike classical push-relabel (which processes one vertex at a time),
Tide operates in global sweeps — each iteration processes all active vertices
at every distance level in two passes (pull then push), followed by a global
relabeling via BFS on the residual graph.

Key properties:
- **Pure functional** — no mutable state, implemented entirely in pure Haskell
- **Exact arithmetic** — uses `Rational` capacities, no floating-point errors
- **Skip-globalRelabel** — tracks residual topology changes to skip redundant BFS passes
- **Verified** — QuickCheck-tested against FGL's max-flow on 10,000 random graphs

Entry point: `pushRelabel :: Network -> Either String ResidualGraph`

## Building

Requires [Stack](https://docs.haskellstack.org/en/stable/):

```
git clone https://github.com/tosku/graph
cd graph
stack build
```

## Testing

```
stack test
```

The test suite includes:
- Unit tests for BFS, DFS, grid construction, Floyd-Warshall, and graph metrics
- Tide max-flow correctness against FGL on a reference network
- QuickCheck property: Tide vs FGL max-flow agreement on 10,000 random graphs

## License

GPL-3
