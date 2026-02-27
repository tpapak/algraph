import qualified TestHS as T
import Test.Graph.AdjacencyList as A
import Test.Graph.AdjacencyList.Grid as G
import Test.Graph.AdjacencyList.BFS as BFS
import Test.Graph.AdjacencyList.DFS as DFS
import Test.Graph.AdjacencyList.WFI as WFI
import Test.Graph.AdjacencyList.Metrics as Met
import Test.Graph.AdjacencyList.PushRelabel.Pure as PRP

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn "Test Begins"
  T.reportTests $
    A.fastTests
      ++ G.fastTests
      ++ BFS.fastTests
      ++ DFS.fastTests
      ++ PRP.fastTests
      ++ WFI.fastTests
      ++ Met.fastTests
  T.reportTestsIO
    Met.ioTests
