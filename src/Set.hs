{-# OPTIONS_GHC -Wunused-top-binds -Wincomplete-patterns -Wtype-defaults #-}
module Set(Tree, insert, contains, delete, listToSet) where

data Tree a = Leaf | Node a (Tree a) (Tree a)

--------------------------------------------------------------------------------

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right

--------------------------------------------------------------------------------

-- delete
delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Node x Leaf Leaf
delete x (Node y left right)
  | x < y     = Node y (delete x left) right
  | x > y     = Node y left (delete x right)
  | otherwise = deleteRoot (Node y left right)

deleteRoot :: Ord a => Tree a -> Tree a
deleteRoot Leaf = Leaf
deleteRoot (Node _ left Leaf) = left
deleteRoot (Node _ Leaf right) = right
deleteRoot (Node _ left right) = Node (findMinElement right) left right

findMinElement :: Ord a => Tree a -> a
findMinElement (Node y Leaf _) = y
findMinElement (Node _ left _) = findMinElement left

--------------------------------------------------------------------------------

contains :: Ord a => a -> Tree a -> Bool
contains _ Leaf = False
contains x (Node y left right)
  | x < y     = contains x left
  | x > y     = contains x right
  | otherwise = True

--------------------------------------------------------------------------------

mapSetToList :: Ord a => Tree a -> (a -> a) -> [a]
mapSetToList Leaf _ = []
mapSetToList (Node x left right) fun = mapSetToList left fun ++ [fun x] ++ mapSetToList right fun

listToSet :: Ord a => [a] -> Tree a
listToSet list = foldr insert Leaf list

inc :: Num a => a -> a
inc x = x + 1

treeToList :: Ord a => Tree a -> [a]
treeToList Leaf = []
treeToList (Node x left right) = treeToList left ++ [x] ++ treeToList right

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let tree = foldr insert Leaf [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    print $ contains (3 :: Int) tree
