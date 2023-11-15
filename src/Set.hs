module Set (Tree, insert, contains, delete, listToSet, mapSet, filterSet, foldlSet, foldrSet, mergeSets, sizeSet, setToList) where

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

--------------------------------------------------------------------------------

instance (Ord a) => Eq (Tree a) where
  tree1 == tree2 =
    sizeSet tree1 == sizeSet tree2
      && all (contains tree1) (setToList tree2)
      && all (contains tree2) (setToList tree1)

--------------------------------------------------------------------------------

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | x > y = Node y left (insert x right)
  | otherwise = Node y left right

--------------------------------------------------------------------------------

-- delete
delete :: (Ord a) => a -> Tree a -> Tree a
delete x Leaf = Node x Leaf Leaf
delete x (Node y left right)
  | x < y = Node y (delete x left) right
  | x > y = Node y left (delete x right)
  | otherwise = deleteRoot (Node y left right)

deleteRoot :: (Ord a) => Tree a -> Tree a
deleteRoot Leaf = Leaf
deleteRoot (Node _ left Leaf) = left
deleteRoot (Node _ Leaf right) = right
deleteRoot (Node _ left right) = Node (findMinElement right) (extractMinElement left) right

findMinElement :: (Ord a) => Tree a -> a
findMinElement Leaf = error "findMinElement(Leaf)"
findMinElement (Node y Leaf _) = y
findMinElement (Node _ left _) = findMinElement left

extractMinElement :: (Ord a) => Tree a -> Tree a
extractMinElement Leaf = error "extractMinElement(Leaf)"
extractMinElement (Node _ Leaf right) = right
extractMinElement (Node x left right) = Node x (extractMinElement left) right

--------------------------------------------------------------------------------

contains :: (Ord a) => Tree a -> a -> Bool
contains Leaf _ = False
contains (Node y left right) x
  | x < y = contains left x
  | x > y = contains right x
  | otherwise = True

--------------------------------------------------------------------------------

listToSet :: (Ord a) => [a] -> Tree a
listToSet = foldr insert Leaf

setToList :: Tree a -> [a]
setToList Leaf = []
setToList (Node x left right) = setToList left ++ [x] ++ setToList right

--------------------------------------------------------------------------------

mapSet :: (Ord a) => (a -> a) -> Tree a -> Tree a
mapSet mapper tree = mapSetRecursive tree Leaf
  where
    mapSetRecursive Leaf mapped = mapped
    mapSetRecursive (Node x left right) mapped =
      let leftResult = mapSetRecursive left (insert (mapper x) mapped)
       in mapSetRecursive right leftResult

filterSet :: (Ord a) => (a -> Bool) -> Tree a -> Tree a
filterSet predicate tree = filterSetRecursive tree Leaf
  where
    filterSetRecursive Leaf filtered = filtered
    filterSetRecursive (Node x left right) filtered =
      let leftResult = filterSetRecursive left (maybeInsert x filtered)
       in filterSetRecursive right leftResult

    maybeInsert x tree' = if predicate x then insert x tree' else tree'

--------------------------------------------------------------------------------

foldrSet :: (a -> a1 -> a1) -> a1 -> Tree a -> a1
foldrSet foldFun begin tree = foldrSetRecursive tree begin
  where
    foldrSetRecursive Leaf result = result
    foldrSetRecursive (Node x left right) result =
      let rightResult = foldrSetRecursive right result
       in foldrSetRecursive left (foldFun x rightResult)

foldlSet :: (a1 -> a -> a1) -> a1 -> Tree a -> a1
foldlSet foldFun begin tree = foldlSetRecursive tree begin
  where
    foldlSetRecursive Leaf result = result
    foldlSetRecursive (Node x left right) result =
      let leftResult = foldlSetRecursive left result
       in foldlSetRecursive right (foldFun leftResult x)

--------------------------------------------------------------------------------

mergeSets :: (Ord a) => Tree a -> Tree a -> Tree a
mergeSets Leaf rhs = rhs
mergeSets lhs Leaf = lhs
mergeSets lhs (Node x left right) =
  let leftResult = mergeSets (insert x lhs) left
   in mergeSets leftResult right

--------------------------------------------------------------------------------

sizeSet :: (Ord a) => Tree a -> Int
sizeSet Leaf = 0
sizeSet (Node _ left right) = sizeSet left + sizeSet right + 1
