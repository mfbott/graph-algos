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
              neighbours = filter ((\(_, distance) -> isNum distance)) -- TODO: Efficiency?
                             (zip [0..] $ lookupRow graph currentNode)


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
  case getDimY graph /= getDimX graph of
    True
      -> error "Maformed matrix."

    _ | outOfRange startNode || outOfRange endNode
        ->
        error "Node index out of range."

      | otherwise
        -> evalState (dijkstraStep (graph, endNode)) (predsAndDists, ooqueue)
  where
    outOfRange :: Index -> Bool
    outOfRange x = 0 > x || x >= getDimX graph

    numberOfNodes = getDimX graph :: Int

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

testgraph1 :: Graph
testgraph1 = fromListEdgeLengths $
  foldl'
  (\xs ((x, y), v) -> ((x, y), v) : ((y, x), v) : xs  )
  []
  [((0,1), Num 40), ((0,2), Num 1), ((2,1), Num 1), ((1,3), Num 4),
   ((1,4), Num 1), ((2,5), Num 9), ((2,6), Num 2), ((4,7), Num 2),
   ((6,7),Num 100)
  ]

testgraph2 :: Graph
testgraph2 = fromListEdgeLengths $
  foldl'
  (\xs ((x, y), v) -> ((x, y), v) : ((y, x), v) : xs  )
  []
  [((0,1), Num 40), ((0,2), Num 1), ((2,1), Num 900), ((1,3), Num 4),
   ((1,4), Num 1), ((2,5), Num 9), ((2,6), Num 2), ((4,7), Num 2),
   ((6,7),Num 100)
  ]

testgraph3 :: Graph
testgraph3 = fromListEdgeLengths $
  foldl'
  (\xs ((x, y), v) -> ((x, y), v) : ((y, x), v) : xs  )
  []
  [((0,1), Num 40),
   ((6,7),Num 100)
  ]
