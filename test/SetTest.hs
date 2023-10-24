import Test.HUnit(assertEqual, runTestTT, Test(..))
import qualified Test.HUnit.Base

import Set(insert, contains, delete, listToSet, filterSet, mergeSets, sizeSet)

main :: IO Test.HUnit.Base.Counts
main = runTestTT tests

tests :: Test
tests = TestList [TestLabel "test1" test1,
                TestLabel "test2" test2,
                TestLabel "test3" test3]

test1 :: Test
test1 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    assertEqual "Test assert" (Set.sizeSet tree) 7
    assertEqual "Test assert" (Set.contains (3 :: Int) tree) True
    assertEqual "Test assert" (Set.contains (5 :: Int) tree) True
    assertEqual "Test assert" (Set.contains (8 :: Int) tree) False

    let insertedTree = Set.insert (8 :: Int) tree
    assertEqual "Test assert" (Set.sizeSet insertedTree) 8
    assertEqual "Test assert" (Set.contains (8 :: Int) insertedTree) True
    assertEqual "Test assert" (Set.sizeSet tree) 7
    assertEqual "Test assert" (Set.contains (8 :: Int) tree) False

    let deletedTree = Set.delete (3 :: Int) tree
    assertEqual "Test assert" (Set.sizeSet deletedTree) 6
    assertEqual "Test assert" (Set.contains (3 :: Int) deletedTree) False
    assertEqual "Test assert" (Set.sizeSet tree) 7
    assertEqual "Test assert" (Set.contains (3 :: Int) tree) True)

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
