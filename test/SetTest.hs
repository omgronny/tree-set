import Test.HUnit(assertEqual, runTestTTAndExit, Test(..))
import Test.QuickCheck
import Set(insert, contains, delete, mapSet, filterSet, mergeSets, sizeSet, listToSet, setToList, filterSet, foldlSet, foldrSet)

main :: IO ()
main = do
    quickCheck prop_insert_delete
    quickCheck prop_filter
    quickCheck prop_merge
    runTestTTAndExit tests

tests :: Test
tests = TestList [TestLabel "test insert delete" test1,
                TestLabel "test filter" test2,
                TestLabel "test merge" test3,
                TestLabel "test map" test4,
                TestLabel "test fold" test5
                ]

test1 :: Test
test1 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    assertEqual "Test assert" (Set.sizeSet tree) 7
    assertEqual "Test assert" (Set.contains tree (3 :: Int)) True
    assertEqual "Test assert" (Set.contains tree (5 :: Int)) True
    assertEqual "Test assert" (Set.contains tree (8 :: Int)) False

    let insertedTree = Set.insert (8 :: Int) tree
    assertEqual "Test assert" (Set.sizeSet insertedTree) 8
    assertEqual "Test assert" (Set.contains insertedTree (8 :: Int)) True
    assertEqual "Test assert" (Set.sizeSet tree) 7
    assertEqual "Test assert" (Set.contains tree (8 :: Int)) False

    let deletedTree = Set.delete (3 :: Int) tree
    assertEqual "Test assert" (Set.sizeSet deletedTree) 6
    assertEqual "Test assert" (Set.contains deletedTree (3 :: Int)) False
    assertEqual "Test assert" (Set.sizeSet tree) 7
    assertEqual "Test assert" (Set.contains tree (3 :: Int)) True)

test2 :: Test
test2 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]

    let filteredZero = Set.filterSet (> 0) tree
    assertEqual "Test assert" (Set.sizeSet filteredZero) 7

    let filtered = Set.filterSet (> 4) tree
    assertEqual "Test assert" (Set.sizeSet filtered) 3)

test3 :: Test
test3 = TestCase ( do
    let tree1 = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    let tree2 = listToSet [3 :: Int,1,10,15,-4,-6]
    assertEqual "Test assert" (Set.sizeSet (Set.mergeSets tree1 tree2)) 11)

test4 :: Test
test4 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]

    let incremented = Set.mapSet (+ 1) tree

    assertEqual "Test assert" (Set.sizeSet incremented) 7
    )

test5 :: Test
test5 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]

    let foldedl = Set.foldlSet (+) 0 tree
    let foldedr = Set.foldrSet (+) 0 tree

    assertEqual "Test assert" foldedl 30
    assertEqual "Test assert" foldedr 30
    )

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
