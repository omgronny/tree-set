import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

import Set(Tree, insert, contains, delete, listToSet)

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [TestLabel "test1" test1]

test1 :: Test
test1 = TestCase ( do
    let tree = listToSet [3 :: Int,1,4,1,5,9,2,6,5,3,5]
    assertEqual "Test assert" (Set.contains (3 :: Int) tree) True
    assertEqual "Test assert" (Set.contains (5 :: Int) tree) True
    assertEqual "Test assert" (Set.contains (8 :: Int) tree) False

    let insertedTree = Set.insert (8 :: Int) tree
    assertEqual "Test assert" (Set.contains (8 :: Int) insertedTree) True
    assertEqual "Test assert" (Set.contains (8 :: Int) tree) False

    let deletedTree = Set.delete 3 :: Int tree
    assertEqual "Test assert" (Set.contains (3 :: Int) deletedTree) False
    assertEqual "Test assert" (Set.contains (3 :: Int) tree) True)
