{-# LANGUAGE InstanceSigs #-}

module Part4.Tasks where

import Util (notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a

infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
  reverse (reversed lst)
  where
    reversed REmpty = []
    reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl helper REmpty
  where
    helper acc el = acc :< el

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
  showsPrec _ lst = showString "[" . showElements lst . showString "]"
    where
      showElements REmpty = showString ""
      showElements (REmpty :< x) = shows x
      showElements (rest :< x1 :< x2) = showElements (rest :< x1) . showString "," . shows x2

  show :: (Show a) => ReverseList a -> String
  show lst = showsPrec 5 lst ""

instance (Eq a) => Eq (ReverseList a) where
  (==) REmpty REmpty = True
  (==) (rest1 :< h1) (rest2 :< h2) = (h1 == h2) && (rest1 == rest2)
  (==) _ _ = False

  (/=) x y = not (x == y)

instance Semigroup (ReverseList a) where
  (<>) l REmpty = l
  (<>) l (t :< h) = (l <> t) :< h

instance Monoid (ReverseList a) where
  mempty = REmpty

instance Functor ReverseList where
  fmap _ REmpty = REmpty
  fmap f (rest :< h) = (fmap f rest) :< (f h)

instance Applicative ReverseList where
  pure x = REmpty :< x
  (<*>) REmpty xs = REmpty
  (<*>) (fs :< f) xs = (fs <*> xs) <> fmap f xs

flatten :: ReverseList (ReverseList a) -> ReverseList a
flatten REmpty = REmpty
flatten (t :< h) = flatten t <> h

instance Monad ReverseList where
  (>>=) lst f = flatten (fmap f lst)
