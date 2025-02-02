import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof, sized)
import BST
import Prelude hiding (lookup) -- Hides built in 'lookup' function
import Data.Type.Coercion (TestCoercion)
import GHC.Conc (BlockReason(BlockedOnSTM))
import Test.QuickCheck (Arbitrary (arbitrary))



-- Executes all test cases
main :: IO ()
main = do
    runTestTT lookupTests
    runTestTT insertTests
    runTestTT listEntriesTests
    quickCheck prop_insert_lookup_found
    return()

-- Empty BST for testing
emptyBST :: BST
emptyBST = Leaf

-- Simple BST for testing
simpleTestBST :: BST
simpleTestBST = InternalNode 5 "Five"
    (InternalNode 4 "Four" Leaf Leaf)
    (InternalNode 6 "Six" Leaf Leaf)

-- Unit Tests for lookup function
lookupTests :: Test
lookupTests = TestList [lookupFoundRoot, lookupFoundLeft, lookupFoundRight, lookupNotFound]

-- Test case: Looking up the root node
lookupFoundRoot :: Test
lookupFoundRoot = TestCase (assertEqual "testLookup Found" (FoundString "Five") (lookup 5 simpleTestBST))

-- Test case: Looking up a left child
lookupFoundLeft :: Test
lookupFoundLeft = TestCase (assertEqual "testLookup Found" (FoundString "Four") (lookup 4 simpleTestBST))

-- Test case: Looking up a right child
lookupFoundRight :: Test
lookupFoundRight = TestCase (assertEqual "testLookup Found" (FoundString "Six") (lookup 6 simpleTestBST))

-- Test case: Looking up a non-existent key
lookupNotFound :: Test
lookupNotFound = TestCase (assertEqual "testLookup NotFound" NotFoundString (lookup 7 simpleTestBST))

-- Unit Tests for insert function
insertTests :: Test
insertTests = TestList [testInsertIntoEmpty, testInsertLeft, testInsertRight, testInsertExisting]

-- Test case: Inserting into an empty BST
testInsertIntoEmpty :: Test
testInsertIntoEmpty = TestCase $ do
    assertEqual "key in empty tree Not Found" NotFoundString (lookup 7 emptyBST)
    let treeAfterInsert = insert 7 "Seven" emptyBST
    assertEqual "key in tree Found" (FoundString "Seven") (lookup 7 treeAfterInsert)

-- Test case: Inserting into the left subtree
testInsertLeft :: Test
testInsertLeft = TestCase $ do
    assertEqual "key in tree Not Found" NotFoundString (lookup 3 simpleTestBST)
    let treeAfterInsert = insert 3 "Three" simpleTestBST
    assertEqual "key in tree Found" (FoundString "Three") (lookup 3 treeAfterInsert)

-- Test case: Inserting into the right subtree
testInsertRight :: Test
testInsertRight = TestCase $ do
    assertEqual "key in tree Not Found" NotFoundString (lookup 7 simpleTestBST)
    let treeAfterInsert = insert 7 "Seven" simpleTestBST
    assertEqual "key in tree Found" (FoundString "Seven") (lookup 7 treeAfterInsert)

-- Test case: Inserting an existing key (should update the value)
testInsertExisting :: Test
testInsertExisting = TestCase $ do
    assertEqual "key in tree Found" (FoundString "Six") (lookup 6 simpleTestBST)
    let treeAfterInsert = insert 6 "SixUpdated" simpleTestBST
    assertEqual "key in tree Found" (FoundString "SixUpdated") (lookup 6 treeAfterInsert)

-- Unit Tests for `listEntries` function
listEntriesTests :: Test
listEntriesTests = TestList [listEntriesEmptyTree, listEntriesSimpleTree]

-- Test case: Listing entries in an empty BST
listEntriesEmptyTree :: Test
listEntriesEmptyTree = TestCase (assertEqual "empty tree" EmptyList (listEntries emptyBST))

-- Test case: Listing entries in a simple BST (should return sorted list)
listEntriesSimpleTree :: Test
listEntriesSimpleTree = TestCase (assertEqual "simple tree" 
    (List [(4,"Four"), (5,"Five"), (6,"Six")]) 
    (listEntries simpleTestBST))

-- Arbitrary instance to generate random BSTs for QuickCheck
instance Arbitrary BST where
    arbitrary = sized randomBST

-- Function to generate a random BST with a given depth
randomBST :: Int -> Gen BST
randomBST 0 = return Leaf  -- Base case: return an empty tree
randomBST n = oneof [
    return Leaf,  -- Option 1: Return an empty tree
    do
        key <- arbitrary  -- Generate a random key
        item <- arbitrary  -- Generate a random value
        left <- randomBST $ n `div` 2  -- Recursively generate left subtree
        right <- randomBST $ n `div` 2  -- Recursively generate right subtree
        return $ InternalNode key item left right  -- Create an internal node
    ]

-- Property test: Inserting a key-value pair into a BST should allow lookup to find it
prop_insert_lookup_found :: Int -> String -> BST -> Bool
prop_insert_lookup_found key value bst =
    lookup key (insert key value bst) == FoundString value



