module Part3.Tasks where

import Data.List(sort, sortBy)
import Data.Ord(comparing, Down(..))
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x:(ff f (f x))

intoDigits :: Int -> [Int]
intoDigits x = helper [] (abs x)
    where
        helper :: [Int] -> Int -> [Int]
        helper acc x =
            let curDigit = x `mod` 10
                nextPart = x `div` 10
                acc' = (curDigit:acc)
            in if nextPart == 0 then acc' else helper acc' nextPart

count :: (Eq a) => (Ord a) => [a] -> [(Int, a)]
count lst = foldl helper [] (sort lst)
    where
        helper :: (Eq a) => [(Int, a)] -> a -> [(Int, a)]
        helper [] x = [(1, x)]
        helper ((n, x):acc) y = if x == y then (n+1, x):acc else (1, y):(n, x):acc

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq [] = error "Empty list"
mostFreq lst =
    let allDigits = lst >>= intoDigits
        counts = count allDigits
        sorted = sortBy (comparing Down) counts
    in case sorted of
        [] -> error "Empty list"
        ((_, x):_) -> x



contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains x (h:t) = (x == h) || contains x t

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = helper [] lst
    where
        helper :: (Eq a) => [a] -> [a] -> [a]
        helper acc [] = acc
        helper acc (x:lst) = if contains x acc then helper acc lst else helper (x:acc) lst

-- Does not preserve order
findAndPop lst pred = helper [] lst
    where
        helper acc [] = (Nothing, acc)
        helper acc (x:t) = if pred x then (Just x, acc ++ t) else helper (x:acc) t


groupByFirst :: (Eq k) => [(k, a)] -> [(k, [a])]
groupByFirst = foldl helper []
    where
        helper :: (Eq k) => [(k, [a])] -> (k, a) -> [(k, [a])]
        helper acc (y, x) =
            case findAndPop acc (\(y', _) -> y' == y) of
                (Nothing, _) -> (y, [x]):acc
                (Just (_, xs), acc) -> (y, x:xs):acc

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst =
    let x_y_pairs = map (\x -> (f x, x)) lst
    in groupByFirst x_y_pairs

