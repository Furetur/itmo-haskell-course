module Part5.Tasks where

import Data.IntMap (partition)
import Util (notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr helper []
  where
    helper el acc = f el : acc

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myConcat . map f

myConcat :: [[a]] -> [a]
myConcat = myFoldl (<>) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr helper []
  where
    helper x acc = if p x then x : acc else acc

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p lst =
  let satisfy = myFilter p lst
      doNotSatisfy = myFilter (not . p) lst
   in (satisfy, doNotSatisfy)
