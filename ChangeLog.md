# Changelog for algraph

- 0.7.0.0
  - Fix source BFS adjacency key mismatch in `residualDistances` — incorrect
    heights on cyclic graphs caused premature termination
  - Replace `Map Edge Int` with `IntMap`-based `resEdgeIndex` for O(log V)
    edge lookup in the hot path (was O(log E))
  - Add skip-globalRelabel optimization: skip BFS when residual topology is
    unchanged (1.25--1.6x speedup)
  - Add QuickCheck test: Tide vs FGL max-flow on 10,000 random graphs
  - Add comprehensive Haddock documentation to all Tide algorithm modules
  - Rewrite README for Hackage submission

- 0.6.0.2
  Updated ghc

- 0.6.0.1
  Updated ghc - removed Data.Natural

- 0.3.2.1
  Documentation on the Push-Relabel Tide algorithm

- 0.3.2.0
  Binary instance of [Edge] so can deserialize Graphs

- 0.3.1.0
  fixed minimum/maximum bug in empty list (Metrics.hs)

- 0.3.0.0
  moved graphviz interface to new package

- 0.2.3.0
  Distance metrics and plot function using graphviz

- 0.2.2.0
  ``DFS`` Added depth first search algorithm

- 0.2.1.1
  ``bfs`` returns empty BFS when source not in graph
