module BST where

--A binary search tree is first and foremost a tree.  The additional
--assumption that data is ordered will be included in the type of the
--functions that will manipulate the tree.

data BST a = Nil | Node a (BST a) (BST a)

--It is convenient to display trees indented according to their
--structure, hence the following explicit Show instance:

instance Show a => Show (BST a) where
    show t = show' t 0
        where show' :: Show a => BST a -> Int -> [Char]
              show' Nil          n = ""
              show' (Node x l r) n = show' r (n+1) ++ 
                                     take (n * 4) [' ',' '..] ++ show x ++ "\n" ++ 
                                     show' l (n+1)

--Insertion is simple: we insert in either the left or the right subtree
--depending on whether the key to be inserted is smaller or larger than
--the root:

insert :: Ord a => a -> BST a -> BST a
insert x Nil          = Node x Nil Nil
insert x (Node y l r) | x <= y    = Node y (insert x l) r
                      | otherwise = Node y l (insert x r)

--Membership is equally simple and follows the same algorithm:

member :: Ord a => a -> BST a -> Bool
member x Nil          = False
member x (Node y l r) | x == y    = True
                      | x <= y    = member x l
                      | otherwise = member x r

--Finding the minimum and maximum values in a tree is just a matter of
--finding the left- or right-most element.  We use an incomplete pattern
--to catch the error cases (when the tree is empty).

findMin, findMax :: Ord a => BST a -> a

findMin (Node y Nil r) = y
findMin (Node y l   r) = findMin l

findMax (Node y l Nil) = y
findMax (Node y l r)   = findMax r

--Obtaining a sorted list out of a BST can be done by traversing the
--tree in inorder.  Note that inorder is applicable to any binary tree
--(not just BST).

inorder :: BST a -> [a]

inorder Nil          = []
inorder (Node y l r) = inorder l ++ [y] ++ inorder r

--Treesort: we take a list, insert the elements one by one in a BST, and
--then produce as output the inorder traversal of the resulting BST.

treesort :: Ord a => [a] -> [a]
treesort l = treesort' l Nil
    where treesort' :: Ord a => [a] -> BST a -> [a]
          treesort' []     t = inorder t
          treesort' (x:xs) t = treesort' xs (insert x t)

--We saved the best for the last.  Deletion in a BST happens like this:
--if either child of the node v to be deleted is empty, then we just
--delete the node; otherwise we find the maximal element v' smaller than
--v (or the minimal element v' larger than v), we store v' in the
--current node, and we delete the old node corresponding to v'.

delete :: Ord a => a -> BST a -> BST a
delete v Nil          = Nil
delete v (Node x l r) | v == x =    delete' (Node x l r)
                      | v <= x =    Node x (delete v l) r
                      | otherwise = Node x l (delete v r)
    where delete' (Node x Nil r) = r
          delete' (Node x l Nil) = l
          delete' (Node x l r) = 
              let maxMin = findMax l 
              in Node maxMin (delete maxMin l) r

--A test tree:

tst :: BST Int
tst = insert 0 (insert 5 (insert 2 (insert 15 (insert 1 (insert 15 (insert 10 Nil))))))
