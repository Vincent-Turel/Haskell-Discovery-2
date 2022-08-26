CS 403: Assignment 2

Vincent Turel
Thomas DiGrande
Julien Whitfield

_________________________________________________________
1. Tree type definition :

> data Tree a = Nil | Node a [Tree a]

Comments :
  We define a tree as being either null or a node composed of a value 'a' and an array of subtrees.

Testing :
    > t0, t1, t2, t3, t4 :: Tree Int
    > t0 = Nil
    > t1 = Node 5 [t0]
    > t2 = Node 6 [t1]
    > t3 = Node 7 [t1,t2]
    > t4 = Node 8 [t3,t3]

NB : These trees will be used for all coming testing.
_________________________________________________________
2. BinTree type definition :

> data BinTree a = BNil | BNode a (BinTree a) (BinTree a)

Comments :
  We define a binary tree as being either null or a node composed of a value 'a' and 2 binary subtrees

Testing :
    > b0, b1, b2, b3, b4 :: BinTree Int
    > b0 = BNil
    > b1 = BNode 5 BNil b0
    > b2 = BNode 6 b1 BNil
    > b3 = BNode 7 b1 b2
    > b4 = BNode 8 b3 b3

NB : These trees will be used for all coming testing.
_________________________________________________________
3. Traces

    - Type class definition

> class Traceable t where
>    traces :: t a -> [[a]]

    - Instantiation for both trees

> instance Traceable BinTree where
>         traces BNil = [[]]
>         traces (BNode x l r) = [] : map (x:) (traces l ++ tail (traces r))

> instance Traceable Tree where
>         traces Nil = [[]]
>         traces (Node x []) = [[], [x]]
>         traces (Node x (y:ys)) = [] : map (x:) (traces y ++ concatMap (tail . traces) ys)

Comments :
  We create Traceable for a general type 't' which returns a 2D array of type 'a'.
  We then instantiate t as a Tree or BinaryTree and traverse the tree recursively to create the trace.

Testing :
  - traces t4           -> [[],[8],[8,7],[8,7,5],[8,7,6],[8,7,6,5],[8,7],[8,7,5],[8,7,6],[8,7,6,5]]
  - traces b4           -> [[],[8],[8,7],[8,7,5],[8,7,6],[8,7,6,5],[8,7],[8,7,5],[8,7,6],[8,7,6,5]]
  - traces t1           -> [[],[5]]
  - traces b2           -> [[],[6],[6,5]]
  - traces b0           -> [[]]

_________________________________________________________
4. Show Eq and Ord instantiation

  - Instantiation

> instance Show a => Show (Tree a) where
>     show t = show' t 0
>         where show' :: Show a => Tree a -> Int -> [Char]
>               show' Nil          _ = ""
>               show' (Node x ys) n = concatMap (\ z -> show' z (n+1)) (snd (tuple ys)) ++
>                                     take (n * 4) [' ',' '..] ++ show x ++ "\n" ++
>                                     concatMap (\ w -> show' w (n+1)) (fst (tuple ys))
>               tuple ys = splitAt ((length ys + 1) `div` 2) ys

> instance Show a => Show (BinTree a) where
>       show t = show' t 0
>           where show' :: Show a => BinTree a -> Int -> [Char]
>                 show' BNil          _ = ""
>                 show' (BNode x l r) n = show' r (n+1) ++
>                                         take (n * 4) [' ',' '..] ++ show x ++ "\n" ++
>                                         show' l (n+1)

> instance Eq a => Eq (Tree a) where
>       (==) t1 t2 = traces t1 == traces t2

> instance Eq a => Eq (BinTree a) where
>       (==) t1 t2 = traces t1 == traces t2

> instance Ord a => Ord (Tree a) where
>       (<=) t1 t2 = traces t1 <= traces t2

> instance Ord a => Ord (BinTree a) where
>       (<=) t1 t2 = traces t1 <= traces t2

Comments :
    Since array are natively orderable and comparable, we can just compare the traces of both tree in order to compare/order them.
    Show works the same way as it does for the bst giiven as exemple in the course.
    For the Tree, we recursively split the array of subtrees in 2 equal halves in order to center correctly the tree.

Testing :
  - t4                      -> show the tree...
  - r4                      -> show the tree...
  - t4 == t3                -> True
  - r4 == r4                -> False
  - r4 >= r4                -> True
  - t3 > t4                 -> False
  - r3 < r4                 -> True


