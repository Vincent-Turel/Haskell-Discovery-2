module Main where

main :: IO ()
main = return ()

data Tree a = Nil | Node a [Tree a]
data BinTree a = BNil | BNode a (BinTree a) (BinTree a)

class Traceable t where
  traces :: t a -> [[a]]

r1, r2, r3, r4 :: Tree Int
r1 = Node 5 [Nil]
r2 = Node 6 [r1]
r3 = Node 7 [r1,r2]
r4 = Node 8 [r1,r3]

b1, b2, b3, b4 :: BinTree Int
b1 = BNode 5 BNil BNil
b2 = BNode 6 b1 BNil
b3 = BNode 7 b1 b2
b4 = BNode 8 b3 b3

--b6 = BNode 1 b8 b9
--b7 = BNode 112 b8 b9
--b8=  BNode 11 BNil BNil
--b9 = BNode 2 BNil BNil

instance Show a => Show (Tree a) where
    show t = show' t 0
        where show' :: Show a => Tree a -> Int -> [Char]
              show' Nil          _ = ""
              show' (Node x ys) n = concatMap (\ z -> show' z n+1) (fst (tuple ys)) ++ showTree x n ++ concatMap (\ w -> show' w n+1) (snd (tuple ys))
              tuple ys = splitAt (length ys `div` 2) ys
              showTree x n = take (n * 4) [' ',' '..] ++ show x ++ "\n"

instance Show a => Show (BinTree a) where
      show t = show' t 0
          where show' :: Show a => BinTree a -> Int -> [Char]
                show' BNil          _ = ""
                show' (BNode x l r) n = show' r (n+1) ++
                                        take (n * 4) [' ',' '..] ++ show x ++ "\n" ++
                                        show' l (n+1)

instance Eq a => Eq (Tree a) where
      (==) t1 t2 = traces t1 == traces t2

instance Eq a => Eq (BinTree a) where
      (==) t1 t2 = traces t1 == traces t2

instance Ord a => Ord (Tree a) where
      (<=) t1 t2 = traces t1 <= traces t2

instance Ord a => Ord (BinTree a) where
      (<=) t1 t2 = traces t1 <= traces t2

instance Traceable BinTree where
        traces BNil = [[]]
        traces (BNode x l r) = [] : map (x:) (traces l ++ tail (traces r))

instance Traceable Tree where
        traces Nil = [[]]
        traces (Node x []) = [[], [x]]
        traces (Node x (y:ys)) = [] : map (x:) (traces y ++ concatMap (tail . traces) ys)