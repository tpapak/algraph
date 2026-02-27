module Test.Graph.AdjacencyList.PushRelabel.FGLComparison where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF

import Test.QuickCheck

import TestHS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Internal
import Data.Graph.AdjacencyList.PushRelabel.Pure (pushRelabel)

-- ================================================================
-- Random network generator
-- ================================================================

data TestNetwork = TestNetwork
  { tnNetwork  :: Network
  , tnNumVerts :: Int
  , tnNumEdges :: Int
  } deriving (Show)

instance Arbitrary TestNetwork where
  arbitrary = do
    n <- choose (3, 20)
    let s = 1
        t = n
    -- Guarantee a path from source to sink
    let pathEdges = [(i, i+1) | i <- [1..n-1]]
    numExtra <- choose (0, n * (n-1) `div` 2)
    extraEdges <- genExtraEdges n numExtra pathEdges
    let allEdgePairs = nub $ pathEdges ++ extraEdges
        es = map (\(u,v) -> Edge u v) allEdgePairs
    caps <- mapM (\_ -> choose (1, 100 :: Int)) allEdgePairs
    let capMap = M.fromList $ zip es (map toRational caps)
        g = graphFromEdges es
        net = Network { graph = g
                       , source = s
                       , sink = t
                       , capacities = capMap
                       , flow = M.empty
                       }
    return $ TestNetwork net n (length allEdgePairs)

  shrink _ = []

genExtraEdges :: Int -> Int -> [(Int,Int)] -> Gen [(Int,Int)]
genExtraEdges _ 0 _ = return []
genExtraEdges n numExtra existing = do
  pairs <- vectorOf (numExtra * 2) $ do
    u <- choose (1, n)
    v <- choose (1, n)
    return (u, v)
  let valid  = filter (\(u,v) -> u /= v) pairs
      unique = nub valid
      new    = filter (`notElem` existing) unique
  return $ take numExtra new

-- ================================================================
-- Convert to FGL
-- ================================================================

networkToFGL :: Network -> (I.Gr () Double, Int, Int)
networkToFGL net =
  let g = graph net
      s = source net
      t = sink net
      vs = map (\v -> (v, ())) $ vertices g
      es = map (\e -> (from e, to e,
                       fromRational $ fromJust $ M.lookup e (capacities net)))
               $ edges g
  in (G.mkGraph vs es, s, t)

-- ================================================================
-- Properties
-- ================================================================

-- | Tide max flow equals FGL max flow
prop_maxFlowMatchesFGL :: TestNetwork -> Property
prop_maxFlowMatchesFGL (TestNetwork net _ _) =
  case pushRelabel net of
    Left err -> counterexample ("pushRelabel failed: " ++ err) False
    Right res ->
      let tideFlow = netFlow res
          (fglGraph, s, t) = networkToFGL net
          fglFlow = toRational (MF.maxFlow fglGraph s t :: Double)
      in counterexample
           ("Tide: " ++ show (fromRational tideFlow :: Double)
            ++ " FGL: " ++ show (fromRational fglFlow :: Double)
            ++ " (" ++ show (length $ vertices $ graph net) ++ " vertices, "
            ++ show (length $ edges $ graph net) ++ " edges)")
           (tideFlow == fglFlow)

-- | pushRelabel never returns Left on valid networks
prop_pushRelabelSucceeds :: TestNetwork -> Property
prop_pushRelabelSucceeds (TestNetwork net _ _) =
  case pushRelabel net of
    Right _ -> property True
    Left err -> counterexample ("pushRelabel failed: " ++ err) False

-- ================================================================
-- Test runner
-- ================================================================

qcCount :: Int
qcCount = 10000

ioTests :: [IO Test]
ioTests =
  [ qcTest "Tide max flow == FGL max flow"  prop_maxFlowMatchesFGL
  , qcTest "pushRelabel succeeds"           prop_pushRelabelSucceeds
  ]

qcTest :: Testable prop => String -> prop -> IO Test
qcTest name prop = do
  result <- quickCheckWithResult stdArgs { maxSuccess = qcCount, chatty = False } prop
  case result of
    Success {} -> return $ testPassed name
                    ("passed (" ++ show qcCount ++ " random graphs)")
    failure    -> return $ testFailed name ("QuickCheck failure", show failure)
