{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Part6.Tasks where

import qualified Data.List
import Data.Map
import Data.Maybe (fromMaybe)
import Util (notImplementedYet)

(|>) x f = f x

outOfBounds () = error "Out Of Bounds"

illegalDim () = error "Illegal dim"

mapFilter f = Prelude.foldr helper []
  where
    helper el acc = case f el of
      Just x -> x : acc
      Nothing -> acc

mapWithIndex f lst = zip [0 ..] lst |> Prelude.map (uncurry f)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix
  { sparseMatrixWidth :: Int,
    sparseMatrixHeight :: Int,
    sparseMatrixElements :: Map (Int, Int) a
  }
  deriving (Show, Eq)

getElementFromSparse :: (Num a1, Ord a2, Ord b) => Map (a2, b) a1 -> (a2, b) -> a1
getElementFromSparse m (i, j) = fromMaybe 0 (Data.Map.lookup (i, j) m)

sparseMapFilterIndices f m =
  let list = Data.Map.toList m
      new_list = mapFilter el_mapper list
   in Data.Map.fromList new_list
  where
    el_mapper (index, value) = fmap (\i -> (i, value)) (f index)

data Decomposition mx = Row [(Int, mx)] | SingleElement Int deriving (Show)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
  zero :: Int -> Int -> mx
  eye :: Int -> mx
  dim :: mx -> (Int, Int)
  multiplyMatrix :: mx -> mx -> mx
  firstRowDecomposition :: mx -> Decomposition mx

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
  zero _ _ = 0

  eye _ = 1

  dim _ = (1, 1)

  multiplyMatrix m1 m2 = m1 * m2

  firstRowDecomposition x = SingleElement x

instance Matrix [[Int]] where
  zero w h = if w > 0 && h > 0 then replicate h (replicate w 0) else illegalDim ()

  eye n = if n > 0 then Prelude.map eye_row [0 .. n - 1] else illegalDim ()
    where
      eye_row i = Prelude.map (\x -> if x == i then 1 else 0) [0 .. n - 1]

  dim (row : rest) = (length row, length rest + 1)

  multiplyMatrix m1 m2 =
    let (x, _) = dim m1
        (_, y) = dim m2
     in if x == y
          then [[sum $ zipWith (*) row1 y | y <- Data.List.transpose m2] | row1 <- m1]
          else illegalDim ()

  firstRowDecomposition m@(first_row : rest) =
    let (w, h) = dim m
     in case (w, h) of
          (1, 1) -> let (x : _) = first_row in SingleElement x
          (w, h) | w == h -> Row [(x, remove_col i) | (x, i) <- zip first_row [0 ..]]
          _ -> illegalDim ()
    where
      remove_col i = [remove_el row i | row <- rest]
      remove_el = helper []
      helper left (cur : right) 0 = Data.List.reverse left ++ right
      helper left (cur : right) i = helper (cur : left) right (i - 1)

instance Matrix (SparseMatrix Int) where
  zero w h = SparseMatrix w h Data.Map.empty

  eye n =
    let els = Prelude.foldl (\acc i -> Data.Map.insert (i, i) 1 acc) Data.Map.empty [0 .. n - 1]
     in SparseMatrix n n els

  dim (SparseMatrix w h _) = (w, h)

  multiplyMatrix :: SparseMatrix Int -> SparseMatrix Int -> SparseMatrix Int
  multiplyMatrix (SparseMatrix w1 h1 m1) (SparseMatrix w2 h2 m2) =
    if h2 == w1
      then SparseMatrix w2 h1 m
      else illegalDim ()
    where
      m = Data.Map.fromList (filterZero lst)
      lst = [((i, j), rowByCol i j) | i <- [0 .. h1 - 1], j <- [0 .. w2 - 1]]
      rowByCol i j = sum $ Prelude.map (\k -> getElementFromSparse m1 (i, k) * getElementFromSparse m2 (k, j)) [0 .. w1 - 1]
      filterZero = Prelude.filter (\(_, x) -> x /= 0)

  firstRowDecomposition (SparseMatrix w h m) =
    case (w, h) of
      (1, 1) -> SingleElement (getElementFromSparse m (0, 0))
      (w, h) | w == h -> Row [(getElementFromSparse m (0, i), SparseMatrix (w - 1) (h - 1) (cut i m)) | i <- [0 .. w - 1]]
      _ -> illegalDim ()
    where
      cut i = cutCol i . cutFirstRow
      cutFirstRow = sparseMapFilterIndices firstRowIndexCutter
      cutCol j = sparseMapFilterIndices (colIndexCutter j)
      firstRowIndexCutter (i, j) = case (i, j) of
        (0, _) -> Nothing
        (i, j) -> Just (i - 1, j)
      colIndexCutter col_j (i, j) = case (i, j) of
        (i, j) | j < col_j -> Just (i, j)
        (i, j) | j == col_j -> Nothing
        (i, j) | j > col_j -> Just (i, j - 1)

-- Определитель матрицы
determinant :: (Matrix m) => m -> Int
determinant m = case firstRowDecomposition m of
  SingleElement x -> x
  Row xs -> alternatingSum $ [el * determinant m' | (el, m') <- xs]
  where
    alternatingSum = sum . mapWithIndex (\i x -> fast_sign i * x)
    fast_sign i = if even i then 1 else -1
