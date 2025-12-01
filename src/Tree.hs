module Tree where

-- Бинарное дерево с явным Empty-конструктором
data Tree a
    = Empty
    | Node (Tree a) a (Tree a)
    deriving (Eq)

-- Пример
myTree :: Tree Int
myTree =
    Node
        ( Node
            (Node Empty (-3) Empty)
            (-2)
            (Node Empty (-1) Empty)
        )
        2
        ( Node
            (Node Empty 3 Empty)
            4
            (Node Empty 5 Empty)
        )

-- Кастомный Show, чтобы выводить как обычный список
instance (Show a) => Show (Tree a) where
    show x = show (toList x)

-- В список (симметричный обход)
toList :: Tree a -> [a]
toList Empty = []
toList (Node l x r) = toList l ++ [x] ++ toList r
