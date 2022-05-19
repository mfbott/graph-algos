module GraphAlgos where

import AdjacencyMatrix

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State


type Index          = Int
type Predecessor    = Maybe Index
type Distance       = InfOrNum Int
type Start          = Index
type End            = Index
type Path           = [Index]

data Graph = AdjacencyMatrix (Matrix EdgeLength)
           | AdjacencyList (V.Vector [(Int, EdgeLength)]) deriving Show


isMalformedInput :: Graph -> Bool
isMalformedInput (AdjacencyMatrix matrix) = getDimY matrix /= getDimX matrix
isMalformedInput (AdjacencyList _) = False -- TODO?


isOutOfRange :: Graph -> Index -> Bool
isOutOfRange (AdjacencyMatrix m) x = 0 > x || x >= getDimX m
isOutOfRange (AdjacencyList v) x = 0 > x || x >= V.length v


getNumberOfNodes :: Graph -> Int
getNumberOfNodes (AdjacencyMatrix matrix) = getDimX matrix
getNumberOfNodes (AdjacencyList v)        = V.length v


getNeighbours :: Graph -> Index -> [(Index, Distance)]
getNeighbours (AdjacencyList v)        currentNode = v V.! currentNode
getNeighbours (AdjacencyMatrix matrix) currentNode = filter
  ((\(_, distance) -> isNum distance))
  (zip [0..] $ lookupRow matrix currentNode)


data OnlyOnceQueue = OnlyOnceQueue { _set :: Set (Distance, Index),
                                     _hasItEverBeenQueued :: VU.Vector Bool,
                                     _notCurrentlyQueued  :: VU.Vector Bool
                                   }

type DijkstraConfig = (Graph, End)
type DijkstraState  = (V.Vector (Predecessor, Distance), OnlyOnceQueue)
type DijkstraValue  = (Path, Distance)


ooqPush :: V.Vector (Predecessor, Distance) -> OnlyOnceQueue
        -> (Distance, Index) -> OnlyOnceQueue
ooqPush currentPredsAndDists ooqueue insertMe@(_, currentNode) =
  case _hasItEverBeenQueued ooqueue VU.! currentNode of
    False
      -> ooqueue { _set = Set.insert insertMe $ _set ooqueue,
                   _hasItEverBeenQueued =
                   _hasItEverBeenQueued ooqueue VU.// [(currentNode, True)],
                   _notCurrentlyQueued  =
                   _notCurrentlyQueued ooqueue VU.// [(currentNode, False)]
                 }

    True | _notCurrentlyQueued ooqueue VU.! currentNode
           -> ooqueue

         | otherwise -- update the element
           -> ooqueue{ _set = Set.insert insertMe
                       $ Set.delete deleteMe $ _set ooqueue }
              where
                deleteMe         = (oldDistance, currentNode)
                (_, oldDistance) = currentPredsAndDists V.! currentNode



ooqPushMultiple :: V.Vector (Predecessor, Distance)
                -> OnlyOnceQueue -> [(Distance, Index)] -> OnlyOnceQueue
ooqPushMultiple v = foldl' (ooqPush v)


ooqPop :: OnlyOnceQueue -> Maybe ((Distance, Index), OnlyOnceQueue)
ooqPop ooqueue
  = case Set.null $ _set ooqueue of
      True
        -> Nothing
      False
        -> Just (smallestTuple, updatedOoqueue)
           where
             (smallestTuple@(_, index), updatedSet) = Set.deleteFindMin $ _set ooqueue
             updatedOoqueue =
               ooqueue { _set = updatedSet,
                         _notCurrentlyQueued =
                         _notCurrentlyQueued ooqueue VU.// [(index, True)]
                       }


dijkstraStep :: DijkstraConfig -> State DijkstraState DijkstraValue
dijkstraStep config@(graph, endNode) = do

  (predsAndDists, ooqueue) <- get

  case ooqPop ooqueue of
    Nothing
      -> return (shortestPath, shortestDist)
         where
           shortestDist :: Distance
           shortestDist = snd $ predsAndDists V.! endNode
           predecessor :: Index -> Predecessor
           predecessor = fst . (V.!) predsAndDists

           shortestPath
             = if shortestDist == Inf then []
               else endNode : walk (predecessor endNode)

           walk :: Predecessor -> Path
           walk Nothing  = []
           walk (Just i) = i : walk (predecessor i)


    Just ((current_d, currentNode), poppedOoqueue)
      -> do let
              updatedPredsAndDists :: V.Vector (Predecessor, Distance)
              updatedPredsAndDists = predsAndDists V.// updatedNeighbours

              updatedOoqueue :: OnlyOnceQueue
              updatedOoqueue = ooqPushMultiple predsAndDists poppedOoqueue
                               ((\(i, (_, d)) -> (d, i)) <$> updatedNeighbours)


              neighbours :: [(Index, Distance)]
              neighbours = getNeighbours graph currentNode


              updatedNeighbours :: [(Index, (Predecessor, Distance))]
              updatedNeighbours = updateNeighbour <$> neighboursThatNeedUpdating
                where
                  updateNeighbour :: (Index, Distance)
                                  -> (Index, (Predecessor, Distance))
                  updateNeighbour (i, distFromHereToNode_i) =
                    (i, (Just currentNode, current_d + distFromHereToNode_i))


              neighboursThatNeedUpdating :: [(Index, Distance)]
              neighboursThatNeedUpdating =
                filter
                (\(i, distFromHereToNode_i) ->
                   currentSummedDist i > current_d + distFromHereToNode_i)
                neighbours

              currentSummedDist :: Index -> Distance
              currentSummedDist = snd . (V.!) predsAndDists


            put (updatedPredsAndDists, updatedOoqueue)
            dijkstraStep config


dijkstra :: Graph -> Start -> End -> (Path, Distance)
dijkstra graph startNode endNode =
  case isMalformedInput graph of
    True
      -> error "Maformed input."

    _ | outOfRange startNode || outOfRange endNode
        ->
        error "Node index out of range."

      | otherwise
        -> evalState (dijkstraStep (graph, endNode)) (predsAndDists, ooqueue)
  where
    outOfRange :: Index -> Bool
    outOfRange = isOutOfRange graph

    numberOfNodes :: Int
    numberOfNodes = getNumberOfNodes graph

    predsAndDists :: V.Vector (Predecessor, Distance)
    predsAndDists =
      (V.fromList $ replicate numberOfNodes (Nothing, Inf))
      V.// [(startNode, (Nothing, Num 0))]

    ooqueue = OnlyOnceQueue {_set = Set.fromList [(Num 0 , startNode)],

                             _hasItEverBeenQueued =
                             (VU.fromList $ replicate numberOfNodes False)
                              VU.// [(startNode, True)],

                             _notCurrentlyQueued =
                             (VU.fromList $ replicate numberOfNodes True)
                              VU.// [(startNode, False)]
                            }



testListe1 :: [((Int, Int), EdgeLength)]
testListe1 =
  foldl'
  (\xs ((x, y), v) -> ((x, y), v) : ((y, x), v) : xs  )
  []
  [((0,1), Num 40), ((0,2), Num 1), ((2,1), Num 1), ((1,3), Num 4),
   ((1,4), Num 1), ((2,5), Num 9), ((2,6), Num 2), ((4,7), Num 2),
   ((6,7),Num 100)
  ]
testListe2 :: [((Int, Int), EdgeLength)]
testListe2 =
  foldl'
  (\xs ((x, y), v) -> ((x, y), v) : ((y, x), v) : xs  )
  []
  [((0,1), Num 40), ((0,2), Num 1), ((2,1), Num 900), ((1,3), Num 4),
   ((1,4), Num 1), ((2,5), Num 9), ((2,6), Num 2), ((4,7), Num 2),
   ((6,7),Num 100)
  ]

testListe3 :: [((Int, Int), EdgeLength)]
testListe3 =
  foldl'
  (\xs ((x, y), v) -> ((x, y), v) : ((y, x), v) : xs  )
  []
  [((0,1), Num 40),
   ((6,7),Num 100)
  ]


testgraph1Matrix :: Graph
testgraph1Matrix = AdjacencyMatrix $ fromListEdgeLengths $ testListe1
testgraph1AdjList :: Graph
testgraph1AdjList = AdjacencyList $ vectorify testListe1

testgraph2Matrix :: Graph
testgraph2Matrix = AdjacencyMatrix $ fromListEdgeLengths $ testListe2
testgraph2AdjList :: Graph
testgraph2AdjList = AdjacencyList $ vectorify testListe2

testgraph3Matrix :: Graph
testgraph3Matrix = AdjacencyMatrix $ fromListEdgeLengths $ testListe3
testgraph3AdjList :: Graph
testgraph3AdjList = AdjacencyList $ vectorify testListe3


vectorify :: [((Int, Int), EdgeLength)]
          -> V.Vector [(Int, EdgeLength)]
vectorify inputList = foldl'
  (\acc ((i1, i2), len) -> acc V.// [(i1, (i2, len) : (acc V.! i1))])
  (V.fromList $ replicate amountOfNodes []) inputList
  where
    amountOfNodes = 1 + findTupleMax inputList 0
    findTupleMax [] acc  = acc -- TODO: Maybe write this in terms of foldl?
    findTupleMax (((x, y),_):xs) acc
      | x >= 0 && y >= 0 = findTupleMax xs (max x (max y acc))
      | otherwise        = error "Nodes out of range."
