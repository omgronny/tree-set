import Set(Tree, insert, contains, delete)

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [TestLabel "test1" test1]

test1 :: Test
test1 = do
    let tree = foldr insert Leaf [3,1,4,1,5,9,2,6,5,3,5]
    TestCase (assertEqual "Test assert" (Set.contains tree 3) True)
    TestCase (assertEqual "Test assert" (Set.contains tree 5) True)
    TestCase (assertEqual "Test assert" (Set.contains tree 8) False)

    let insertedTree = Set.insert 8 tree
    TestCase (assertEqual "Test assert" (Set.contains insertedTree 8) True)
    TestCase (assertEqual "Test assert" (Set.contains tree 8) False)

    let deletedTree = Set.delete 3 tree
    TestCase (assertEqual "Test assert" (Set.contains deletedTree 3) False)
    TestCase (assertEqual "Test assert" (Set.contains tree 3) True)
