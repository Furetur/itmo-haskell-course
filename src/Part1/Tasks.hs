module Part1.Tasks where

import qualified Data.List as List
import Util (notImplementedYet)

aux n acc = if n == 0 then acc else aux (n - 1) (acc * n)

fact n = aux n 1

repeatIndex start_i f =
  let el = f start_i
   in let tail = repeatIndex (start_i + 1) f
       in el : tail

n :: Int
n = 100

z :: Int
z = 0

inperiod x
  | x < 0 = inperiod (x + 2 * pi)
  | x > 4 * pi = inperiod (x - 2 * pi)
  | otherwise = x

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x =
  let y = inperiod x
   in let series = repeatIndex z (\i -> ((-1) ^ i) * y ^^ (2 * i + 1) / fact (2 * fromIntegral i + 1))
       in sum (take n series)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x =
  let y = inperiod x
   in let series = repeatIndex z (\i -> ((-1) ^ i) * y ^^ (2 * i) / fact (2 * fromIntegral i))
       in sum (take n series)

-- наибольший общий делитель двух чисел

absolute x = if x < 0 then -x else x

myGCD :: Integer -> Integer -> Integer
myGCD x y = if y == 0 then absolute x else myGCD y (x `mod` y)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?

isLeapYear :: Integer -> Bool
isLeapYear year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 = False
  | year `mod` 4 == 0 = True
  | otherwise = False

daysInMonth year month
  | month `elem` [4, 6, 9, 11] = 30
  | month == 2 = if isLeapYear year then 29 else 28
  | otherwise = 31

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year
  | year < 0 = False
  | month <= 0 || month > 12 = False
  | day <= 0 || day > daysInMonth year month = False
  | otherwise = True

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x n = if n == 0 then 1 else x * myPow x (n - 1)

-- является ли данное число простым?

-- returns true if Exists x, low <= x <= hi ^ x | div
checkDivisors low hi div
  | low > hi = False
  | div `mod` low == 0 = True
  | otherwise = checkDivisors (low + 1) hi div

isPrime :: Integer -> Bool
isPrime n = not (checkDivisors 2 (n - 1) n)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат

-- Area = 0.5 * abs(sum(xi*yi+1 - xi+1*yi))

gaussSum :: [Point2D] -> Double -> Double
gaussSum [] acc = acc
gaussSum [x] acc = acc
gaussSum ((x_a, y_a) : (x_b, y_b) : pts) acc = gaussSum ((x_b, y_b) : pts) (acc + x_a * y_b - x_b * y_a)

shapeArea :: [Point2D] -> Double
-- shapeArea pts = 0.5 * abs(gaussSum pts 0)
shapeArea pts =
  let (xs, ys) = unzip pts
   in let sum1 = sum $ zipWith (*) xs (tail ys ++ [head ys])
       in let sum2 = sum $ zipWith (*) ys (tail xs ++ [head xs])
           in 0.5 * abs (sum1 - sum2)

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник

greatestSide a b c
  | a >= b && a >= c = (a, b, c)
  | b >= a && b >= c = (b, a, c)
  | c >= a && c >= b = (c, a, b)

almostEqual x y = abs (x - y) <= 0.0001

triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c =
  let (x, y, z) = greatestSide a b c
   in let cosine = (y ^ 2 + z ^ 2 - x ^ 2) / (2 * y * z)
       in if x > y + z
            then -1
            else
              if cosine `almostEqual` 0
                then 2
                else
                  if cosine > 0
                    then 1
                    else 0
