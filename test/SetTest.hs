import Test.HUnit ( assertEqual, runTestTTAndExit, Test(..) )

import Set(Tree, insert, contains, delete, listToSet)

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [TestLabel "test1" test1]

test1 :: Test
test1 = TestCase ( do
    let tree = listToSet [3,1,4,1,5,9,2,6,5,3,5]
    assertEqual "Test assert" (Set.contains tree 3) True
    assertEqual "Test assert" (Set.contains tree 5) True
    assertEqual "Test assert" (Set.contains tree 8) False

    let insertedTree = Set.insert 8 tree
    assertEqual "Test assert" (Set.contains insertedTree 8) True
    assertEqual "Test assert" (Set.contains tree 8) False

    let deletedTree = Set.delete 3 tree
    assertEqual "Test assert" (Set.contains deletedTree 3) False
    assertEqual "Test assert" (Set.contains tree 3) True)
