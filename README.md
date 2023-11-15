# Лабораторная работа 2

## Реализация

```haskell
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)

...

```

### Тесты

``` haskell

prop_insert_delete :: [Int] -> Int -> Bool
prop_insert_delete list elementToInsert = do
    let tree = listToSet list
    let inserted = insert elementToInsert tree

    if contains tree elementToInsert then
        tree == inserted
    else
        tree == delete elementToInsert inserted

prop_filter :: [Int] -> Int -> Bool
prop_filter list threshold = do
    let tree = listToSet list
    let filtered = filterSet (> threshold) tree

    all (> threshold) (setToList filtered) &&
        all (contains filtered) (filter (> threshold) list)

prop_merge :: [Int] -> [Int] -> Bool
prop_merge list1 list2 = do
    let tree1 = listToSet list1
    let tree2 = listToSet list2

    let merged = mergeSets tree1 tree2

    all (contains merged) (setToList tree1) &&
        all (contains merged) (setToList tree2)

prop_merge_assoc :: [Int] -> [Int] -> [Int] -> Bool
prop_merge_assoc list1 list2 list3 = do
    let tree1 = listToSet list1
    let tree2 = listToSet list2
    let tree3 = listToSet list3

    let merged1 = mergeSets (mergeSets tree1 tree2) tree3
    let merged2 = mergeSets tree1 (mergeSets tree2 tree3)

    all (contains merged1) (setToList merged2) &&
        all (contains merged2) (setToList merged1)
```



