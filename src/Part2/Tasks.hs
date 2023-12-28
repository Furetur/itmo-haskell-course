module Part2.Tasks where

import Util (notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term
  = IntConstant {intValue :: Int} -- числовая константа
  | Variable {varName :: String} -- переменная
  | BinaryTerm {op :: BinaryOp, lhv :: Term, rhv :: Term} -- бинарная операция
  deriving (Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus

(|-|) :: Term -> Term -> Term
(|-|)= BinaryTerm Minus

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

infixl 1 |+|
infixl 1 |-|
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable name) = if varName == name then replacement else Variable name
replaceVar varName replacement (IntConstant i) = IntConstant i
replaceVar varName replacement (BinaryTerm op l r) = BinaryTerm op (replaceVar varName replacement l) (replaceVar varName replacement r)

calc :: BinaryOp -> Int -> Int -> Int
calc Plus = (+)
calc Minus = (-)
calc Times = (*)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm op l r) =
  let l' = evaluate l
      r' = evaluate r
   in case (l', r') of
        (IntConstant i1, IntConstant i2) -> IntConstant (calc op i1 i2)
        _ -> BinaryTerm op l' r'
evaluate x = x
