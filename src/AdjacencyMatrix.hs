module AdjacencyMatrix (Matrix, updateMatrixByCoords, lookup, lookupRow,
                        lookupColumn, getDimX, getDimY, dimension,
                        AdjacencyMatrix.toList, fromListEdgeLengths,
                        minEdge, maxEdge, allVertices, InfOrNum, isNum
                       )
where


import Prelude hiding (lookup)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.List (intercalate)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State


data Matrix a = Matrix !Int !Int (V.Vector a) deriving (Eq, Ord)

indexOf :: Matrix a -> Int -> Int -> Int
indexOf (Matrix dim_x _ _) x y = x + y * dim_x

lookup :: Int -> Int -> Matrix a -> a
lookup x y m@(Matrix _ _ v) = (V.!) v $ indexOf m x y

updateMatrix :: Matrix a -> [(Int, a)] -> Matrix a
updateMatrix (Matrix dim_x dim_y v) xs = Matrix dim_x dim_y (v V.// xs)


updateMatrixByCoords :: Matrix a -> [((Int, Int), a)] -> Matrix a
updateMatrixByCoords matrix = updateMatrix matrix . fmap
  (\(tupl, x) -> ( (uncurry . indexOf) matrix tupl, x))


lookupRow :: Matrix a -> Int -> [a]
lookupRow m@(Matrix dim_x _ _) y = [lookup x y m | x <- [0..dim_x-1]]


lookupColumn :: Matrix a -> Int -> [a]
lookupColumn m@(Matrix _ dim_y _) x = [lookup x y m | y <- [0..dim_y-1]]


instance Show a => Show (Matrix a) where
  show m@(Matrix _ dim_y _) =
    intercalate "\n" $ show <$> [lookupRow m y | y <- [0..dim_y-1]]


data InfOrNum a = Num !a | Inf | MinusInf deriving (Show, Eq)


isNum :: InfOrNum a -> Bool
isNum (Num _) = True
isNum _       = False


instance Ord a => Ord (InfOrNum a) where
  compare (Num _) Inf       = LT
  compare Inf (Num _)       = GT
  compare Inf Inf           = EQ
  compare (Num x) (Num y)   = compare x y
  compare (Num _) MinusInf  = GT
  compare MinusInf (Num _)  = LT
  compare MinusInf MinusInf = EQ
  compare Inf MinusInf      = GT
  compare MinusInf Inf      = LT

instance Functor InfOrNum where
  fmap _ Inf      = Inf
  fmap _ MinusInf = MinusInf
  fmap f (Num x)  = Num $ f x

instance Applicative InfOrNum where
  pure = Num
  (<*>) _ Inf      = Inf
  (<*>) _ MinusInf = MinusInf
  (<*>) (Num fx) (Num y) = Num $ fx y
  (<*>) Inf _      = Inf
  (<*>) MinusInf _ = MinusInf

instance (Eq a, Num a) => Num (InfOrNum a) where
  negate (Num x) = Num $ negate x
  negate Inf = MinusInf
  negate MinusInf = Inf
  (+) (Num x) (Num y) = Num $ x + y
  (+) Inf Inf = Inf
  (+) MinusInf MinusInf = MinusInf
  (+) Inf MinusInf = error "Undefined operation."
  (+) MinusInf Inf = Inf + MinusInf
  (+) (Num x) notNum = if x == abs x then notNum else negate notNum
  (+) notNum r@(Num _) = r + notNum

  (-) (Num x) (Num y) = Num $ x - y
  (-) Inf Inf = error "Undefined operation."
  (-) MinusInf MinusInf = MinusInf
  (-) Inf MinusInf = error "Undefined operation."
  (-) MinusInf Inf = error "Undefined operation."
  (-) notNum (Num _) = notNum
  (-) (Num _) _      = MinusInf

  (*) _ _ = error "Not implemented."
  fromInteger = pure . fromInteger
  abs MinusInf = Inf
  abs Inf      = Inf
  abs (Num x)  = Num $ abs x
  signum (Num x) = Num $ signum x
  signum Inf = Num 1
  signum MinusInf = Num (-1)


type Index          = Int
type Predecessor    = Maybe Index
type Distance       = InfOrNum Int
type EdgeLength     = InfOrNum Int
type Graph          = Matrix EdgeLength
type Start          = Index
type End            = Index
type Path           = [Index]


dimension :: Ord a => [((Int, Int), a)] -> Int
dimension = (+) 1 . (foldl' (\xs ((a, b), _) -> max a (max b xs)) 0)

getDimX :: Matrix a -> Int
getDimX (Matrix dim_x _ _) = dim_x

getDimY :: Matrix a -> Int
getDimY (Matrix _ dim_y _) = dim_y


toList :: Matrix a -> [((Int, Int), a)]
toList m@(Matrix dim_x dim_y _) =
  [ ((x, y), lookup x y m) | x <- [0..dim_x-1], y <- [0..dim_y-1] ]



fromListEdgeLengths :: [((Int, Int), EdgeLength)] -> Matrix EdgeLength
fromListEdgeLengths edgesList = updateMatrixByCoords startMatrix edgesList
  where
    dim :: Int
    dim = dimension edgesList
    startMatrix :: Matrix EdgeLength
    startMatrix = Matrix dim dim $ V.fromList $ replicate (dim*dim) Inf


maxEdge :: Ord a => Matrix a -> ((Int, Int), a)
maxEdge = foldl1
  (\x@((_, _), c') y@((_, _), c) -> if c' > c then x else y)
  . AdjacencyMatrix.toList

minEdge :: Ord a => Matrix a -> ((Int, Int), a)
minEdge = foldl1
  (\x@((_, _), c') y@((_, _), c) -> if c' < c then x else y)
  . AdjacencyMatrix.toList


allVertices :: Matrix a -> Set Int
allVertices (Matrix dim_x dim_y _)
  = if dim_x /= dim_y
    then error "incompatible Matrix"
    else Set.fromList [0..dim_x-1]



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
