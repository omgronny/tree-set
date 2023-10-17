import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

import Set(Tree, insert, contains, delete, listToSet, mapSet, filterSet, foldlSet, foldrSet, mergeSets, sizeSet)

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [TestLabel "test1" test1]

test1 :: Test
test1 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    assertEqual "Test assert" (Set.sizeSet tree) 7 :: Int
    assertEqual "Test assert" (Set.contains (3 :: Int) tree) True
    assertEqual "Test assert" (Set.contains (5 :: Int) tree) True
    assertEqual "Test assert" (Set.contains (8 :: Int) tree) False

    let insertedTree = Set.insert (8 :: Int) tree
    assertEqual "Test assert" (Set.sizeSet insertedTree) 8 :: Int
    assertEqual "Test assert" (Set.contains (8 :: Int) insertedTree) True
    assertEqual "Test assert" (Set.sizeSet tree) 7 :: Int
    assertEqual "Test assert" (Set.contains (8 :: Int) tree) False

    let deletedTree = Set.delete (3 :: Int) tree
    assertEqual "Test assert" (Set.sizeSet deletedTree) 6 :: Int
    assertEqual "Test assert" (Set.contains (3 :: Int) deletedTree) False
    assertEqual "Test assert" (Set.sizeSet tree) 7 :: Int
    assertEqual "Test assert" (Set.contains (3 :: Int) tree) True)


test2 :: Test
test2 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]

    let filteredZero = Set.filterSet (> 0) tree
    assertEqual "Test assert" (Set.sizeSet filteredZero) 7 :: Int

    let filtered = Set.filterSet (> 4) tree
    assertEqual "Test assert" (Set.sizeSet filtered) 3 :: Int)