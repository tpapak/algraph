# algraph

A pure Haskell graph library using adjacency list representation, featuring
the **Tide algorithm** — a level-synchronous push-pull-relabel solver for the
maximum flow problem.

## Quick start

```haskell
import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Pure
import Data.Graph.AdjacencyList.PushRelabel.Internal (netFlow, stCut)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  -- Build a directed graph: 1 -> 2 -> 4
  --                         1 -> 3 -> 4
  let g = graphFromEdges [Edge 1 2, Edge 1 3, Edge 2 4, Edge 3 4]
      caps = M.fromList [ (Edge 1 2, 10), (Edge 1 3, 5)
                        , (Edge 2 4, 8),  (Edge 3 4, 7) ]
      net = Network { graph = g, source = 1, sink = 4
                    , capacities = caps, flow = M.empty }
  case pushRelabel net of
    Left err -> putStrLn $ "Error: " ++ err
    Right rg -> do
      putStrLn $ "Max flow: " ++ show (netFlow rg)       -- 13
      putStrLn $ "Min cut:  " ++ show (stCut rg)         -- s-t cut edges
```

## Features

- **Maximum flow** via the Tide algorithm — the only push-relabel
  implementation in the Haskell ecosystem, and the only sub-O(VE^2) pure
  functional max-flow solver available
- **Exact arithmetic** — capacities and flows use `Rational`, guaranteeing
  correct results for arbitrary inputs (no floating-point rounding)
- **s-t minimum cut** extracted directly from the max-flow residual graph
- **BFS and DFS** with level maps, parent maps, spanning trees, topological
  sort, longest path, and connectivity queries
- **Floyd-Warshall** all-pairs shortest paths (weighted and unweighted)
- **Graph metrics** — eccentricity, radius, diameter, density
- **d-dimensional lattice generator** — cubic lattices with periodic boundary
  conditions (toroidal topology) in arbitrary dimension
- **QuickCheck-verified** — Tide tested against FGL on 10,000 random graphs

## Modules

| Module | Description |
|--------|-------------|
| `Data.Graph.AdjacencyList` | Core types (`Vertex`, `Edge`, `Graph`, `Neighbors`, `EdgeMap`) and graph constructors |
| `Data.Graph.AdjacencyList.Network` | Flow network type (`Network`, `Capacities`, `Capacity = Rational`) |
| `Data.Graph.AdjacencyList.PushRelabel.Pure` | **Tide algorithm** — `pushRelabel :: Network -> Either String ResidualGraph` |
| `Data.Graph.AdjacencyList.PushRelabel.Internal` | Residual graph types, `netFlow`, `stCut`, push/pull primitives |
| `Data.Graph.AdjacencyList.BFS` | Breadth-first search |
| `Data.Graph.AdjacencyList.DFS` | Depth-first search, topological sort, longest path |
| `Data.Graph.AdjacencyList.WFI` | Floyd-Warshall all-pairs shortest paths |
| `Data.Graph.AdjacencyList.Metrics` | Eccentricity, radius, diameter, density |
| `Data.Graph.AdjacencyList.Grid` | d-dimensional cubic lattices with periodic boundary conditions |

## The Tide algorithm

Each iteration ("tide") performs three global sweeps on the residual graph:

1. **globalRelabel** — BFS from sink (and source) to recompute vertex heights
2. **globalPull** — right fold over active vertices: pull flow on reverse edges
3. **globalPush** — left fold over active vertices: push flow on forward edges

The algorithm terminates when both the net flow and the set of overflowing
vertices stabilize. A **skip-globalRelabel** optimization tracks whether any
edge crosses a saturation boundary during push/pull; when none do, the BFS
is skipped (1.25--1.6x speedup in practice).

### Complexity

| | Worst case | Practical |
|---|---|---|
| **Tides** | O(V^2) | O(V) |
| **Per-tide** | O((V+E) log V) | O((V+E) log V) |
| **Total** | O(V^2 (V+E) log V) | O(V(V+E) log V) |

The log V factor comes from `IntMap` lookups in the pure Haskell
implementation. The O(V^2) worst case requires exponentially-varying
capacities; on graphs with polynomially-bounded capacity ratios (covering
virtually all practical inputs), the tide count is empirically O(V).

### How it compares

| Algorithm | Best known complexity |
|---|---|
| Edmonds-Karp (FGL) | O(VE^2) |
| Dinic | O(V^2 E) |
| Highest-label push-relabel | O(V^2 sqrt(E)) |
| **Tide (as implemented)** | **O(V(V+E) log V)** practical |

A companion Rust implementation ([tide-maxflow](https://github.com/tpapak/tide-maxflow))
achieves O(VE) practical complexity using O(1) array-based data structures and
has been benchmarked against Hi-PR, Google OR-Tools, LEMON, and Boost on 63
DIMACS graph instances.

## Context in the Haskell graph ecosystem

| Library | Max flow | Shortest paths | Metrics | Generators |
|---|---|---|---|---|
| **containers** | -- | -- | -- | -- |
| **fgl** | Edmonds-Karp O(VE^2) | Dijkstra, BF | -- | -- |
| **algebraic-graphs** | -- | -- | -- | -- |
| **algraph** | **Tide O(V(V+E) log V)** | **Floyd-Warshall APSP** | eccentricity, radius, diameter, density | d-dim lattices (PBC) |

Key differences from fgl:

- **Faster max flow** — Tide is asymptotically better than fgl's Edmonds-Karp
  at all graph densities
- **Exact arithmetic** — fgl uses `Double` for max flow; algraph uses
  `Rational`
- **Faster traversals** — algraph's BFS/DFS are O((V+E) log V) vs fgl's
  O(V^2) due to fgl's O(V)-per-vertex `match` decomposition. At 1M vertices,
  algraph BFS is 3.5x faster
- **APSP** — Floyd-Warshall is built in; fgl only offers single-source
  algorithms

fgl has broader algorithm coverage (SCC, dominators, MST, Dijkstra,
transitive closure) and supports labeled nodes/edges.

## Building

Requires [Stack](https://docs.haskellstack.org/en/stable/):

```
git clone https://github.com/tpapak/algraph
cd algraph
stack build
```

## Testing

```
stack test
```

The test suite includes:

- Unit tests for graph construction, BFS, DFS, grid lattices, Floyd-Warshall,
  and graph metrics
- Tide max-flow correctness test against FGL on a reference network
- QuickCheck property: Tide vs FGL max-flow agreement on 10,000 random graphs

## License

LGPL-3 -- see [LICENSE](LICENSE).
