-- {-# OPTIONS_GHC -Wunused-top-binds -Wincomplete-patterns -Wtype-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Set(Tree, insert, contains, delete, listToSet, mapSet, filterSet, foldlSet, foldrSet, mergeSets, sizeSet) where

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
deleteRoot (Node _ left right) = Node (findMinElement right) (extractMinElement left) right

findMinElement :: Ord a => Tree a -> a
findMinElement (Node y Leaf _) = y
findMinElement (Node _ left _) = findMinElement left

extractMinElement :: Ord a => Tree a -> Tree a
extractMinElement (Node _ Leaf right) = right
extractMinElement (Node x left right) = Node x (extractMinElement left) right

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
listToSet = foldr insert Leaf

setToList :: Ord a => Tree a -> [a]
setToList Leaf = []
setToList (Node x left right) = setToList left ++ [x] ++ setToList right

--------------------------------------------------------------------------------

mapSet :: Ord a => Tree a -> (a -> a) -> Tree a
mapSet tree fun = listToSet (mapSetToList tree fun)

filterSet :: Ord a => (a -> Bool) -> Tree a -> Tree a
filterSet predicate tree = listToSet (filter predicate (setToList tree))

--------------------------------------------------------------------------------

foldrSet :: Ord a => (a -> a1 -> a1) -> a1 -> Tree a -> a1
foldrSet foldFun begin tree = foldr foldFun begin (setToList tree)

foldlSet :: Ord a => (a1 -> a -> a1) -> a1 -> Tree a -> a1
foldlSet foldFun begin tree = foldl foldFun begin (setToList tree)

--------------------------------------------------------------------------------

mergeSets :: Ord a => Tree a -> Tree a -> Tree a
mergeSets Leaf rhs = rhs
mergeSets lhs Leaf = lhs
mergeSets lhs rhs = insertList lhs (setToList rhs)

insertList :: Ord a => Tree a -> [a] -> Tree a
insertList Leaf list = listToSet list
insertList tree [] = tree
insertList tree (h:t) = insertList (insert h tree) t

--------------------------------------------------------------------------------

sizeSet :: Ord a => Tree a -> Int
sizeSet Leaf = 0
sizeSet tree = length (setToList tree)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    let deletedTree = Set.delete (3 :: Int) tree
    print $ sizeSet deletedTree
