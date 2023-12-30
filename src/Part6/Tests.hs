module Part6.Tests where

import qualified Data.Map
import Data.Maybe (fromMaybe)
import Part6.Tasks
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

unit_eye = do
  eye 1 @?= one
  eye 1 @?= [[one]]
  eye 1 @?= SparseMatrix 1 1 (Data.Map.fromList [((0, 0), one)])
  eye 2 @?= [[one, 0], [0, one]]
  eye 2 @?= SparseMatrix 2 2 (Data.Map.fromList [((0, 0), one), ((1, 1), one)])
  where
    one :: Int; one = 1

unit_zero = do
  zero 1 1 @?= zz
  zero 2 1 @?= [[zz, zz]]
  zero 2 2 @?= [[zz, zz], [zz, zz]]
  zero 5 5 @?= SparseMatrix 5 5 (Data.Map.fromList ([] :: [((Int, Int), Int)]))
  where
    zz :: Int; zz = 0

unit_multiplyMatrix_list = do
  (z `multiplyMatrix` o) @?= z
  (o `multiplyMatrix` z) @?= z
  (o `multiplyMatrix` o) @?= o
  where
    z :: [[Int]]
    z = zero d d
    o :: [[Int]]
    o = eye d
    d = 100

unit_multiplyMatrix_sparse = do
  (z `multiplyMatrix` o) @?= z
  (o `multiplyMatrix` z) @?= z
  (o `multiplyMatrix` o) @?= o
  where
    z :: SparseMatrix Int
    z = zero d d
    o :: SparseMatrix Int
    o = eye d
    d = 100

sparseToList (SparseMatrix w h els) =
  let row_indices = [0 .. (h - 1)]
      col_indices = [0 .. (w - 1)]
      get_element row col = Data.Map.lookup (row, col) els |> fromMaybe 0
      make_row i = Prelude.map (get_element i) col_indices
   in Prelude.map make_row row_indices

listToSparse matrix =
  SparseMatrix
    { sparseMatrixWidth = length (head matrix),
      sparseMatrixHeight = length matrix,
      sparseMatrixElements = Data.Map.fromList [((i, j), val) | (row, i) <- zip matrix [0 ..], (val, j) <- zip row [0 ..], val /= 0]
    }

diag :: [Int] -> [[Int]]
diag list = [[if col_j == row_i then x else 0 | (_, col_j) <- zip list [0 ..]] | (x, row_i) <- zip list [0 ..]]

someMatrix :: [Int] -> [[Int]]
someMatrix list = [[if col_j == row_i then x + row_i else col_j | (_, col_j) <- zip list [0 ..]] | (x, row_i) <- zip list [0 ..]]

prop_multiplication_of_list_and_sparse_is_the_same lst =
  listToSparse (a `multiplyMatrix` b) == listToSparse a `multiplyMatrix` listToSparse b
  where
    lst' = 1 : lst
    a :: [[Int]]
    a = diag lst'
    b = someMatrix lst'

prop_determinant_of_int_is_itself i = determinant i == i

unit_list_eye_determinant = determinant e @=? 1
  where
    e :: [[Int]]; e = eye 5

unit_list_diag_determinant = determinant m @=? product lst
    where lst = [1..8]
          m = diag lst

unit_sparse_diag_determinant = determinant m @=? product lst
    where lst = [1..10]
          m = listToSparse (diag lst)
