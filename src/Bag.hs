{-# LANGUAGE InstanceSigs #-}

module Bag (
    Bag,
    empty,
    singleton,
    insert,
    insertCount,
    deleteOne,
    deleteAll,
    member,
    count,
    size,
    distinctSize,
    toList,
    fromList,
    mapBag,
    filterBag,
    foldrBag,
    foldlBag,
) where

import Data.List (foldl')
import Tree (Tree (..))

-- ghci> let bag = singleton 2
-- ghci> let bag1 = insert 2 bag
-- ghci> let bag2 = insert 1 bag1
-- ghci> let bag3 = insert 5 bag2
-- ghci> bag3
-- ghci> let bag4 = insert 4 bag3
-- ghci> bag4

-- Узел дерева: значение и количество его вхождений
data Entry a = Entry a Int
    deriving (Eq)

-- Кастомный Show для Entry
instance (Show a) => Show (Entry a) where
    show (Entry x c) = "(" ++ show x ++ "," ++ show c ++ ")"

newtype Bag a = Bag (Tree (Entry a))
    deriving (Show)

instance (Ord a) => Eq (Bag a) where
    a == b = entriesTree a == entriesTree b

-- Вспомогательные функции для Entry
-- Получения ключа (элемента) из Entry a
entryKey :: Entry a -> a
entryKey (Entry x _) = x

-- Получения числа вхождений конкретной Entry a
entryCount :: Entry a -> Int
entryCount (Entry _ c) = c

-- Создание пустого Bag
empty :: Bag a
empty = Bag Empty

-- Создание Bag с одним элементом
singleton :: a -> Bag a
singleton x = Bag (Node Empty (Entry x 1) Empty)

-- Вставка с указанием кратности
insertCount :: (Ord a) => a -> Int -> Bag a -> Bag a
insertCount x n (Bag t) = Bag (go t)
  where
    go Empty = Node Empty (Entry x n) Empty
    go (Node l e r) =
        case compare x (entryKey e) of
            LT -> Node (go l) e r
            GT -> Node l e (go r)
            EQ -> Node l (Entry (entryKey e) (entryCount e + n)) r

-- Вставка одного элемента в Bag
insert :: (Ord a) => a -> Bag a -> Bag a
insert x = insertCount x 1

-- Удалить одну копию элемента
deleteOne :: (Ord a) => a -> Bag a -> Bag a
deleteOne x (Bag t) = Bag (go t)
  where
    go Empty = Empty
    go (Node l e r) =
        case compare x (entryKey e) of
            LT -> Node (go l) e r
            GT -> Node l e (go r)
            EQ ->
                case entryCount e of
                    c | c > 1 -> Node l (Entry (entryKey e) (c - 1)) r
                    _ -> merge l r

-- Удалить все копии элемента
deleteAll :: (Ord a) => a -> Bag a -> Bag a
deleteAll x (Bag t) = Bag (go t)
  where
    go Empty = Empty
    go (Node l e r) =
        case compare x (entryKey e) of
            LT -> Node (go l) e r
            GT -> Node l e (go r)
            EQ -> merge l r

-- Слияние двух деревьев при удалении
merge :: Tree a -> Tree a -> Tree a
merge Empty r = r
merge l Empty = l
merge l r = Node l minNode rest
  where
    (minNode, rest) = extractMin r

-- Поиск минимального элемента в дереве
extractMin :: Tree a -> (a, Tree a)
extractMin (Node Empty v r) = (v, r)
extractMin (Node l v r) =
    let (m, l') = extractMin l
     in (m, Node l' v r)
extractMin Empty = error "extractMin on empty tree"

-- Проверка наличия элемента x в Bag
member :: (Ord a) => a -> Bag a -> Bool
member x = (> 0) . count x

-- Подсчет количества вхождений элемента x в Bag
count :: (Ord a) => a -> Bag a -> Int
count x (Bag t) = go t
  where
    go Empty = 0
    go (Node l e r) =
        case compare x (entryKey e) of
            LT -> go l
            GT -> go r
            EQ -> entryCount e

-- Подсчет общего числа элементов в Bag
size :: Bag a -> Int
size (Bag t) = go t
  where
    go Empty = 0
    go (Node l e r) = go l + entryCount e + go r

-- Подсчет числа различных элементов в Bag
distinctSize :: Bag a -> Int
distinctSize (Bag t) = go t
  where
    go Empty = 0
    go (Node l _ r) = go l + 1 + go r

-- Преобразование Bag в список элементов
toList :: Bag a -> [a]
toList (Bag t) = go t
  where
    go Empty = []
    go (Node l e r) = go l ++ replicate (entryCount e) (entryKey e) ++ go r

-- Создание Bag из списка элементов
fromList :: (Ord a) => [a] -> Bag a
fromList = foldl' (flip insert) empty

-- Маппинг функции по всем элементам Bag
mapBag :: (Ord b) => (a -> b) -> Bag a -> Bag b
mapBag f (Bag t) = foldl' (\acc (k, c) -> insertCount (f k) c acc) empty (entries t)

-- Фильтрация Bag по предикату
filterBag :: (a -> Bool) -> Bag a -> Bag a
filterBag p (Bag t) = Bag (go t)
  where
    go Empty = Empty
    go (Node l e r)
        | p (entryKey e) = Node (go l) e (go r)
        | otherwise = merge (go l) (go r)

-- Правосторонняя свертка Bag
foldrBag :: (a -> b -> b) -> b -> Bag a -> b
foldrBag f z bag = foldr f z (toList bag)

-- Левосторонняя свертка Bag
foldlBag :: (b -> a -> b) -> b -> Bag a -> b
foldlBag f z bag = foldl' f z (toList bag)

-- Полугруппа через объединение
instance (Ord a) => Semigroup (Bag a) where
    (<>) :: (Ord a) => Bag a -> Bag a -> Bag a
    (<>) a b = foldl' (\acc (k, c) -> insertCount k c acc) a (entriesTree b)

-- Моноид для Bag
instance (Ord a) => Monoid (Bag a) where
    mempty = empty
    mappend = (<>)

-- Вспомогательные обходы
entries :: Tree (Entry a) -> [(a, Int)]
entries Empty = []
entries (Node l e r) = entries l ++ [(entryKey e, entryCount e)] ++ entries r

-- Получение списка (элемент, количество) из Bag
entriesTree :: Bag a -> [(a, Int)]
entriesTree (Bag t) = entries t
