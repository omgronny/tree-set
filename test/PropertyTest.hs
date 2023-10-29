import Test.QuickCheck
import Test.HUnit(assertEqual, runTestTTAndExit, Test(..))
import Set(insert, contains, delete, mapSet, filterSet, mergeSets, sizeSet, listToSet, filterSet, foldlSet, foldrSet)

-- main :: IO ()
-- main = runTestTTAndExit tests

-- tests :: TestTree
-- tests = testGroup "tests"
--     [
--     testGroup "HUnit tests" [testEmpty, testSize, testHeight, testFind, testDelete, testFilter],
--     testGroup "QC tests" [qcTestInsert]
--     ]

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (ys++xs) == reverse xs ++ reverse ys

main = quickCheck prop_revapp

