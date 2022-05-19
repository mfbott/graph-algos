module AdjacencyMatrix where

import Prelude hiding (lookup)
import qualified Data.Vector as V
import Data.List (intercalate)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set


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
  (*) _ _ = error "Not implemented."
  fromInteger = pure . fromInteger
  abs MinusInf = Inf
  abs Inf      = Inf
  abs (Num x)  = Num $ abs x
  signum (Num x) = Num $ signum x
  signum Inf = Num 1
  signum MinusInf = Num (-1)


type EdgeLength     = InfOrNum Int
type Graph          = Matrix EdgeLength


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
