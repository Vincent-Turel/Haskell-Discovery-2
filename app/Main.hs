module Main where

main :: IO ()
main = return ()

data Tree2 a = Pair a [Tree2 a]

data Tree a = Nil | Node a [Tree a]
data BinTree a = BNil | BNode a (BinTree a) (BinTree a)

class Traceable t where
  traces :: t a -> [[a]]

t0, t1, t2, t3, t4, t5 :: Tree Int
t0 = Nil
t1 = Node 5 [t0]
t2 = Node 6 [t1]
t3 = Node 7 [t1,t2]
t4 = Node 8 [t3,t3,t3,t3]
t5 = Node 8 [t3,t3,t3,t3,t3]

b0, b1, b2, b3, b4 :: BinTree Int
b0 = BNil
b1 = BNode 5 BNil b0
b2 = BNode 6 b1 BNil
b3 = BNode 7 b1 b2
b4 = BNode 8 b3 b3


instance Show a => Show (Tree a) where
    show t = show' t 0
        where show' :: Show a => Tree a -> Int -> [Char]
              show' Nil          _ = ""
              show' (Node x ys) n = concatMap (\ z -> show' z (n+1)) (snd (tuple ys)) ++ 
                                    take (n * 4) [' ',' '..] ++ show x ++ "\n" ++ 
                                    concatMap (\ w -> show' w (n+1)) (fst (tuple ys))
              tuple ys = splitAt ((length ys + 1) `div` 2) ys

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

multiples :: Int -> Int -> [Int]
multiples m n = [x | x <- [1..], mod x m == 0, mod x n == 0]

lcm1 :: Int -> Int -> Int
lcm1 x y = head (multiples x y)

concat1 :: [[a]] -> [a]
concat1 = foldr (++) []

  breadthFirst :: Tree2 a -> [a]
  breadthFirst (Pair a []) = [a]
  breadthFirst (Pair a ys) =  [a] ++ concatMap breadthFirst ys