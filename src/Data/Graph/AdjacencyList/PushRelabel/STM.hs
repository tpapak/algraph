{-|
Module      : PushRelabel - STM
Description : Maximum Flow - Min Cut - Push relabel algorithm with concurrency
Copyright   : Thodoris Papakonstantinou, 2017-2026
License     : LGPL-3
Maintainer  : dev@tpapak.com
Stability   : experimental
Portability : POSIX


 -}


module Data.Graph.AdjacencyList.PushRelabel.STM
  (
    --ResidualGraph (..)
  {-, pushRelabel-}
  {-, netFlow-}
  ) where

{-
import Data.List
import Data.Maybe
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import qualified Control.Monad.Parallel as Par
import Control.Concurrent.STM
import Control.Monad

import Graph
import Graph.Network
import Graph.BFS

import Graph.PushRelabel.Internal


data ResidualVertexSTM = ResidualVertexSTM Vertex (Level,Level) (TVar Height) (TVar Excess)
  deriving (Eq)

type ResidualVerticesSTM = IM.IntMap ResidualVertexSTM

data ResidualEdgeSTM = ResidualEdgeSTM Edge Capacity (TVar Flow)
  deriving (Eq)

type ResidualEdgesSTM = IM.IntMap ResidualEdgeSTM

type OverflowingSTM = IM.IntMap (TVar Set.IntSet)

data ResidualGraphSTM = ResidualGraphSTM 
  { network :: Network
  , netVertices :: ResidualVerticesSTM
  , netEdges :: ResidualEdgesSTM
  , netNeighborsMap :: NeighborsMap 
  , overflowing :: OverflowingSTM
  , steps :: Int
  }
  deriving (Eq)

initializeResidualGraph :: Network -> IO ResidualGraph
initializeResidualGraph net = do
  vs <- initializeVertices net
  es <- initializeEdges net
  fovs <- getForwardOverflowing net
  bovs <- getBackwardOverflowing net
  let neimap = getNetNeighborsMap $ graph net 
  return ResidualGraph { network = net
                       , netVertices = vs
                       , netEdges = es
                       , netNeighborsMap = neimap
                       , foverflowing = fovs
                       , boverflowing = bovs
                       , steps = 0
                       }

reverseNetwork :: Network -> Network
reverseNetwork net = Network { graph = reverseGraph $ graph net
                             , source = sink net
                             , sink = source net
                             , capacities = reverseCapacities $ capacities net
                             , flow = reverseFlows $ flow net
                             }

getNetNeighborsMap :: Graph -> NeighborsMap
getNetNeighborsMap g =
  let revneis = getReverseNeighbors (vertices g) (edges g)
      neis v = (neighbors g v, fromJust (IM.lookup v revneis))
   in foldl (\ac v -> IM.insert v (neis v) ac) IM.empty (vertices g)

netNeighbors :: NeighborsMap -> Vertex -> ([Vertex],[Vertex]) -- ^ graph and reverse (inward and outward) neighbors
netNeighbors nm v = fromJust $ IM.lookup v nm

sourceEdges :: Network -> [(Edge,Capacity)]
sourceEdges net = 
  let g = graph net
      cs = capacities net
      s = source net
      cap v = fromJust $ M.lookup (Edge s v) cs
    in map (\v -> ((Edge s v), cap v )) (neighbors g s) 

initializeVertices :: Network -> IO ResidualVertices
initializeVertices net = do
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      sh = fromIntegral $ numVertices g
      ses = sourceEdges net
      vs = vertices $ graph net
      es = edges $ graph net
      flevels = BFS.level $ BFS.bfs (graph net) (source net)
      blevels = BFS.level $ BFS.bfs (reverseGraph $ graph net) (sink net)
      fl v = fromJust $ IM.lookup v flevels
      bl v = fromJust $ IM.lookup v blevels
      zvs = IM.fromList $ 
              zip (vertices g) (map (\v -> 
                if v == t 
                   then ResidualVertexP t (fl v, bl v) 0 0 
                   else ResidualVertex v (fl v, bl v) 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> 
                    let v = to e
                     in (cx-c, IM.adjust (const (ResidualVertex v (fl v, bl v) 0 c)) v ac)
                         ) (0, zvs) ses
      fvs = IM.insert s (ResidualVertex s (0,sh) sh sx) nvs
  traverse (\(ResidualVertexP v h x) -> do 
                                      th <- newTVarIO h
                                      tx <- newTVarIO x
                                      return $ ResidualVertex v th tx
           ) fvs

initializeEdges :: Network -> IO ResidualEdges
initializeEdges net = do
  let g = graph net
  let cs = capacities net
  let s = source net
  let t = sink net
  let inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdgeP e c 0)) (M.toList cs)
  let ses = sourceEdges net
  let fes = foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdgeP e c c) ac) inites ses 
  traverse (\(ResidualEdgeP e c f) -> do
                                      tf <- newTVarIO f
                                      return $ ResidualEdge e c tf
           ) fes

overflowing :: ResidualGraph -> IO [Vertex]
overflowing rg = atomically $ do
  let vs = vertices $ graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  xvs <- mapM (\v -> fmap ((,) v) (excess rg v)) vs
  return $ map fst $ filter (\(v,x) -> x/=0 && v /= s && v /= t) xvs

pushRelabel :: Network -> IO (Either String ResidualGraph)
pushRelabel net = do
  initg <- initializeResidualGraph net
  ovfs <- fmap Set.fromList $ overflowing initg
  res <- prl initg ovfs 0
  let satreds = saturatedReverseNeighbors (snd res)
  let sinkvs = Set.fromList $ topSort $ adjBFS satreds t
  let sourcevs = Set.difference (Set.fromList $ vertices g) sinkvs
  print "source vertices"
  print sourcevs
  print "sink vertices"
  print sinkvs
  {-let minset = minimumBy (\a b -> compare (Set.size a)  (Set.size b)) [sourcevs,sinkvs]-}
  let minset = sourcevs
  let stcut vs = Set.foldl' (\ac v -> ac + foldl' (\ac' n -> 
                          case Set.member n vs  of
                            True -> ac'
                            False -> ac' + fromJust (M.lookup (Edge v n) cs)
                            ) 0 (neighbors g v))
                0 vs :: Rational
  print "ST cut"
  print $ fromRational $ stcut sourcevs
  print $ fromRational $ stcut sinkvs
  !revg <- initializeResidualGraph $ reverseNetwork net
  !ovfs' <- fmap Set.fromList $ overflowing revg
  !res' <- prl revg ovfs' 0
  let !satreds' = saturatedReverseNeighbors (snd res')
  let !sinkvs' = Set.fromList $ topSort $ adjBFS satreds' s
  let !sourcevs' = Set.difference (Set.fromList $ vertices g) sinkvs'
  print "ST cut reverse"
  print $ fromRational $ stcut sourcevs'
  print $ fromRational $ stcut sinkvs'
  print "reverse source vertices"
  print sourcevs'
  print "reverse sink vertices"
  print sinkvs'
  revflow <- netFlow revg 
  print "reverse flow"
  print (fromRational $ revflow :: Double)
  return $ Right initg
  where 
      g = graph net
      cs = capacities net
      s = source net
      t = sink net
      vs = vertices g
      innervs = filter (\v -> v/=s && v/=t) $ vertices g
      prl :: ResidualGraph -> Set.IntSet -> Int -> IO ([ResidualVertexP],[ResidualEdgeP])
      prl rg ovfs steps =
        if Set.null ovfs 
           then do
             es <- atomically $ mapM (\e -> do
                     let c = edgeCapacity rg e
                     f <- edgeFlow rg e 
                     return $ ResidualEdgeP e c f
                                     ) 
                   (edges (graph (network rg)))
             vs <- atomically $ mapM (\v -> do
                     h <- height rg v 
                     x <- excess rg v 
                     return $ ResidualVertexP v h x
                                     ) 
                   (vertices (graph (network rg)))
             !ovfs' <- overflowing rg
             if null ovfs' 
                then do
                  return (vs,es)
                else do
                  prl rg (Set.fromList ovfs') (steps + 1)
                  {-print ovfs'-}
                  {-mapM_ (\v-> atomically $ pulldischarge rg v) ovfs'-}
                  return (vs,es)
             {-return (vs,es)-}
           else do
             !lovfs <- Par.mapM (\v -> do
                           !os <- atomically $
                             do
                               orElse 
                                 (discharge rg v)
                                 ((relabel rg v) >> 
                                   orElse 
                                     (discharge rg v)
                                     (return [])
                                 )
                                 {-!x <- excess rg v-}
                                 {-if x > 0 -}
                                    {-then do-}
                                       {-orElse -}
                                         {-(discharge rg v)-}
                                         {-((relabel rg v) >> discharge rg v)-}
                                    {-else return []-}
                           return os
                                ) $ Set.toList ovfs
             let !ovfs' = Set.difference (Set.unions $ map Set.fromList lovfs) $ Set.fromList [s,t]
             {-!ovfs' <- fmap Set.fromList $ overflowing rg-}
             {-if steps > fromIntegral t -}
                {-then do-}
                 {-ovfs'' <- fmap Set.fromList $ overflowing rg-}
                 {-print steps-}
                 {-print $ Set.difference ovfs' ovfs''-}
               {-else return ()-}
             prl rg ovfs' (steps +1)
                  
discharge :: ResidualGraph -> Vertex -> STM [Vertex]
discharge g v = do
  let neimap = netNeighborsMap g
  let (fns, rns) = fromJust $ IM.lookup v neimap
  let feds = map (\n -> fromTuple (v,n)) fns
  let reds = map (\n -> fromTuple (n,v)) rns
  !changed <- foldM (\ac e -> fmap (\mv -> 
        case mv of 
          Nothing -> ac
          Just v -> v:ac) (push g e)) [] feds
  !changed' <- foldM (\ac e -> fmap (\mv -> 
        case mv of 
          Nothing -> ac
          Just v -> v:ac) (pull g e)) changed reds
  if null changed' then 
                retry
              else
                return changed

push :: ResidualGraph -> Edge -> STM (Maybe Vertex)
push g e = do 
  let u = from e
  let v = to e
  hu <- height g u
  hv <- height g v
  xu <- excess g u 
  xv <- excess g v
  let c = edgeCapacity g e
  f <- edgeFlow g e
  let nvs = netVertices g
  let xf = min xu (c - f)
  if (hu == hv + 1) && xf > 0 then do
    updateEdge g e (f + xf)
    updateVertex g u hu (xu - xf)
    updateVertex g v hv (xv + xf)
    return $ Just v
  else return Nothing

pull :: ResidualGraph -> Edge -> STM (Maybe Vertex)
pull g e = do 
  let u = from e
  let v = to e
  hu <- height g u
  hv <- height g v
  xu <- excess g u 
  xv <- excess g v
  let c = edgeCapacity g e
  f <- edgeFlow g e
  let nvs = netVertices g
  let xf = min xv f
  if (hv == hu + 1)  && xf > 0 then do 
    updateEdge g e (f - xf)
    updateVertex g u hu (xu + xf)
    updateVertex g v hv (xv - xf)
    return $ Just u
  else return Nothing

relabel :: ResidualGraph -> Vertex -> STM ()
relabel g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let gnes = map (\n -> fromTuple (v,n)) $ fst ns -- ^ neighboring graph edges
  let rnes = map (\n -> fromTuple (n,v)) $ snd ns -- ^ neighboring reverse edges
  gacfs <- mapM (\e -> fmap ((\f -> (e,(-) (edgeCapacity g e) f))) (edgeFlow g e)) gnes
  let gcfs = filter (\(e,cf) -> cf > 0) gacfs
  racfs <- mapM (\e -> fmap ((\f -> (e,f))) (edgeFlow g e)) rnes
  let rcfs = filter (\(e,cf) -> cf > 0) racfs
  hv <- height g v
  xv <- excess g v
  neighborHeights <- (++) <$> (mapM (\(e,c) -> height g (to e)) gcfs) <*> mapM (\(e,c) -> height g (from e)) rcfs
  let newh = 1 + minimum neighborHeights
  case any (\nh -> hv > nh) neighborHeights || null neighborHeights of
     False -> do
       updateVertex g v newh xv
     True -> return ()

saturatedReverseNeighbors :: [ResidualEdgeP] -> IM.IntMap [Vertex]
saturatedReverseNeighbors es = 
  let res = filter (\(ResidualEdgeP e c f) -> f < c) es
   in foldl' (\ac (ResidualEdgeP e c f) -> let u = from e
                                               v = to e
                                               mns = IM.lookup v ac
                                            in case mns of
                                                 Nothing -> IM.insert v [u] ac
                                                 Just ns -> IM.insert v (u:ns) ac
             ) IM.empty res

updateVertex :: ResidualGraph -> Vertex -> Height -> Excess -> STM ()
updateVertex g v nh nx = do 
  let nv = fromJust $ IM.lookup v (netVertices g)
      s = source $ network g
      t = sink $ network g
  if v == s || v == t then do
    return ()
  else do
    updateHeight nv nh
    updateExcess nv nx

updateHeight :: ResidualVertex -> Height -> STM ()
updateHeight (ResidualVertex v h x) nh = writeTVar h nh

updateExcess :: ResidualVertex -> Excess -> STM ()
updateExcess (ResidualVertex v h x) nx = writeTVar x nx

updateEdge :: ResidualGraph -> Edge -> Flow -> STM ()
updateEdge g e f = do
  let l = graph $ network g
  let eid = fromJust $ edgeIndex l e
  let es = netEdges g
  let (ResidualEdge oe c tf) = fromJust $ IM.lookup eid es
  writeTVar tf f

netFlow :: ResidualGraph -> IO Flow
netFlow g = atomically $ do
  fl <- inflow g (sink (network g))
  return fl

sourceFlow :: ResidualGraph -> IO Flow
sourceFlow g = atomically $ do
  fl <- outflow g (source (network g))
  return fl

vertex :: ResidualVertex -> Vertex
vertex (ResidualVertex v _ _) = v

height :: ResidualGraph -> Vertex -> STM Height
height rg v = do 
  let g = graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  let nvs = fromIntegral $ numVertices g
  let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices rg)
  if v == s 
     then do
        return nvs
      else
        if v == t 
           then do
             return 0
            else
              readTVar h

excess :: ResidualGraph -> Vertex -> STM Excess
excess rg v = do 
  let g = graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  let nvs = fromIntegral $ numVertices g
  let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices rg)
  if v == s 
     then do
        return 0
      else
        if v == t 
           then do
             return 0
            else
              readTVar e


edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex (graph $ network g) e) (netEdges g)
                    in c

edgeFlow :: ResidualGraph -> Edge -> STM Flow
edgeFlow g e = do
  let l = graph $ network g
  let (ResidualEdge ne c tf) = fromJust $ IM.lookup (fromJust $ edgeIndex l e) (netEdges g)
  readTVar tf

printVertex :: ResidualGraph -> Vertex -> IO ()
printVertex rg v = do
  (h ,x ,inf ,ouf ) <- atomically $ do
    x' <- excess rg v
    h' <- height rg v
    inf' <- inflow rg v
    ouf' <- outflow rg v
    return (h',x',inf',ouf')
  putStrLn $ show v 
    ++ " h: "++ show h 
    ++ ", exc: " ++ show (fromRational x) 
    ++ ", infl: " ++ show (fromRational inf)
    ++ ", outf: " ++ show (fromRational ouf) 

printEdge :: ResidualGraph -> Edge -> IO ()
printEdge rg e = do
  (c,f) <- atomically $ do
    let c' = edgeCapacity rg e
    f' <- edgeFlow rg e
    return (c',f')
  putStrLn $ show e 
    ++ " c: "++ show (fromRational c) 
    ++ ", f: " ++ show (fromRational f) 

inflowP :: ResidualGraph -> [ResidualEdgeP] -> ResidualVertexP -> Double
inflowP g es (ResidualVertexP v h x) =
  let ns  = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (n,v)) $ snd ns
      xf = foldl' (\ac e ->
                            let (ResidualEdgeP e'' c f) = fromJust $ find (\(ResidualEdgeP e' c f) -> e' == e ) es
                             in (f + ac)
                  ) 0 reds
               in fromRational xf

outflowP :: ResidualGraph -> [ResidualEdgeP] -> ResidualVertexP -> Double
outflowP g es (ResidualVertexP v h x) =
  let ns  = netNeighbors (netNeighborsMap g) v 
      feds = map (\n -> fromTuple (v,n)) $ fst ns
      xf = foldl' (\ac e ->
                            let (ResidualEdgeP e'' c f) = fromJust $ find (\(ResidualEdgeP e' c f) -> e' == e ) es
                             in (f + ac)
                  ) 0 feds
               in fromRational xf

inflow :: ResidualGraph -> Vertex -> STM Flow
inflow g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let reds = map (\n -> fromTuple (n,v)) $ snd ns
  foldM (\ac e -> ((+) ac) <$> edgeFlow g e) 0 reds 

outflow :: ResidualGraph -> Vertex -> STM Flow
outflow g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let feds = map (\n -> fromTuple (v,n)) $ fst ns
  foldM (\ac e -> ((+) ac) <$> edgeFlow g e) 0 feds 
  -}
