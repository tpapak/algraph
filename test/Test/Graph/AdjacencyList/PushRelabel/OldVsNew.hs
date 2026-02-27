module Test.Graph.AdjacencyList.PushRelabel.OldVsNew where

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Lazy as IM

import Test.QuickCheck

import TestHS

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Internal
import Data.Graph.AdjacencyList.PushRelabel.Pure       (pushRelabel)
import Data.Graph.AdjacencyList.PushRelabel.PureOld    (pushRelabelOld)

-- ================================================================
-- Test network generator
-- ================================================================

data TestNetwork = TestNetwork
  { tnNetwork  :: Network
  , tnNumVerts :: Int
  , tnNumEdges :: Int
  } deriving (Show)

instance Arbitrary TestNetwork where
  arbitrary = do
    n <- choose (3, 15)
    let s = 1
        t = n
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
-- Properties: compare old (M.Map Edge) vs new (IntMap) implementation
-- ================================================================

-- | Old and new produce the exact same max flow value.
prop_oldNewSameMaxFlow :: TestNetwork -> Property
prop_oldNewSameMaxFlow (TestNetwork net _ _) =
  case (pushRelabelOld net, pushRelabel net) of
    (Right old, Right new) ->
      let oldFlow = netFlow old
          newFlow = netFlow new
      in counterexample
           ("Old flow: " ++ show (fromRational oldFlow :: Double)
            ++ " New flow: " ++ show (fromRational newFlow :: Double))
           (oldFlow == newFlow)
    (Left errO, Left errN) ->
      counterexample
        ("Both failed: old=" ++ errO ++ " new=" ++ errN)
        True
    (Left errO, Right _) ->
      counterexample ("Old failed: " ++ errO ++ ", New succeeded") False
    (Right _, Left errN) ->
      counterexample ("Old succeeded, New failed: " ++ errN) False

-- | Old and new produce the same number of tides (steps).
prop_oldNewSameSteps :: TestNetwork -> Property
prop_oldNewSameSteps (TestNetwork net _ _) =
  case (pushRelabelOld net, pushRelabel net) of
    (Right old, Right new) ->
      let oldSteps = steps old
          newSteps = steps new
      in counterexample
           ("Old steps: " ++ show oldSteps
            ++ " New steps: " ++ show newSteps)
           (oldSteps == newSteps)
    _ -> property True

-- | Old and new produce identical per-edge flows.
prop_oldNewSameEdgeFlows :: TestNetwork -> Property
prop_oldNewSameEdgeFlows (TestNetwork net _ _) =
  case (pushRelabelOld net, pushRelabel net) of
    (Right old, Right new) ->
      let g  = graph $ network old
          es = edges g
          mismatches = filter (\e ->
            edgeFlow old e /= edgeFlow new e) es
      in counterexample
           ("Mismatched edge flows on " ++ show (length mismatches)
            ++ " of " ++ show (length es) ++ " edges: "
            ++ show (map (\e ->
              ( e
              , fromRational (edgeFlow old e) :: Double
              , fromRational (edgeFlow new e) :: Double
              )) mismatches))
           (null mismatches)
    _ -> property True

-- | resEdgeIndex matches edgeIndex for all edges.
prop_resEdgeIndexMatchesEdgeIndex :: TestNetwork -> Property
prop_resEdgeIndexMatchesEdgeIndex (TestNetwork net _ _) =
  let g  = graph net
      rg = initializeResidualGraph net
      nm = netNeighborsMap rg
      es = edges g
      failures = filter (\e ->
        edgeIndex g e /= resEdgeIndex nm e) es
  in counterexample
       ("Mismatched indices on " ++ show (length failures)
        ++ " of " ++ show (length es) ++ " edges")
       (null failures)

-- ================================================================
-- Test runner: 1000 random graphs each
-- ================================================================

qcCount :: Int
qcCount = 1000

ioTests :: [IO Test]
ioTests =
  [ qcTest "old vs new: same max flow"   prop_oldNewSameMaxFlow
  , qcTest "old vs new: same steps"      prop_oldNewSameSteps
  , qcTest "old vs new: same edge flows" prop_oldNewSameEdgeFlows
  , qcTest "edgeIndex == resEdgeIndex"   prop_resEdgeIndexMatchesEdgeIndex
  ]

qcTest :: Testable prop => String -> prop -> IO Test
qcTest name prop = do
  result <- quickCheckWithResult stdArgs { maxSuccess = qcCount, chatty = False } prop
  case result of
    Success {} -> return $ testPassed name
                    ("passed (" ++ show qcCount ++ " random graphs)")
    failure    -> return $ testFailed name ("QuickCheck failure", show failure)
