module BST where

-- Hides built-in function `lookup`
import Prelude hiding (lookup)

-- Type aliases
data KeyType = Int
data ItemType = String

-- Defining Binary search tree (BST) data structure
data BST 
    = InternalNode Int String BST BST -- Node with key, value, left subtree, right subtree
    | Leaf  -- Leaf node representing an empty tree
    deriving (Show)

-- Data structure which can handle all lookups (found or not found)
data MaybeString 
    = FoundString String  -- Successful lookup
    | NotFoundString -- Unsuccessful lookup
    deriving (Eq, Show)

-- Searches for a specific key in BST
lookup :: Int -> BST -> MaybeString
lookup soughtKey Leaf = NotFoundString -- If tree/subtree is empty, return not found
lookup soughtKey (InternalNode key item leftChild rightChild) = 
    if soughtKey == key
        then FoundString item -- Key found, return key
    else if soughtKey < key
        then lookup soughtKey leftChild -- Search left subtree
    else
        lookup soughtKey rightChild -- Search right subtree
        
-- Inserts new key into BST
insert :: Int -> String -> BST -> BST
insert newKey newItem Leaf = InternalNode newKey newItem Leaf Leaf -- If tree/subtree is empty, create new node 
insert newKey newItem (InternalNode key item leftChild rightChild) =
    if newKey < key
        then InternalNode key item (insert newKey newItem leftChild) rightChild -- Insert key as new left subtree
    else if newKey > key
        then InternalNode key item leftChild (insert newKey newItem rightChild) -- Insert key as new right subtree
    else
        InternalNode newKey newItem leftChild rightChild -- Replace the value if key already exists

-- Represents a list of BST entries
data MaybeList 
    = List [(Int,String)] -- A list of key-value pairs
    | EmptyList -- Represents an empty list of key-value pairs
    deriving (Eq, Show)

-- Returns all key-value pairs in BST in sorted order
listEntries :: BST -> MaybeList
listEntries Leaf = EmptyList -- If tree/subtree is empty, return empty list
listEntries tree = List (listEntriesWorker tree) -- Convert BST to sorted list

-- Recursive helper function to traverse BST in order
listEntriesWorker :: BST -> [(Int, String)]
listEntriesWorker Leaf = [] -- Return empty list
listEntriesWorker (InternalNode key item leftChild rightChild) =
    listEntriesWorker leftChild ++ [(key, item)] ++ listEntriesWorker rightChild 
    -- ^ Recursively collect values from left subtree, current node, right subtree