module Data.BST where 

import qualified Test.QuickCheck as QC
import qualified Data.List       as L

-------------------------------------------------------------------------------
-- | BST data type 
-------------------------------------------------------------------------------

data BST a
  = Leaf                      -- ^ empty tree
  | Node a (BST a) (BST a)    -- ^ node with left and right subtrees
  deriving (Show)

-- | Binary-Search Ordering Invariant

isOrdered :: (Ord a) => BST a -> Bool
isOrdered Leaf         = True 
isOrdered (Node e l r) = forall l (\x -> x < e) -- all elts in `l` are less    than `e`
                      && forall r (\x -> e < x) -- all elts in `r` are greater than `e` 
                      && isOrdered l            -- left subtree `isOrdered`
                      && isOrdered r            -- right subtree `isOrdered` 

forall :: BST a -> (a -> Bool) -> Bool
forall Leaf         _ = True
forall (Node e l r) p = p e && forall l p && forall r p

-------------------------------------------------------------------------------
-- | The empty BST
-------------------------------------------------------------------------------
empty :: BST a
empty = Leaf 


-------------------------------------------------------------------------------
-- | Build a tree from a list
-------------------------------------------------------------------------------
build :: (Ord a) => [a] -> BST a

build xs = builds xs Leaf
  where
    builds [] tree = tree
    builds (a:as) tree = builds as (add a tree)

-------------------------------------------------------------------------------
-- | Check membership in BST
-------------------------------------------------------------------------------
contains :: (Ord a) => a -> BST a -> Bool

contains x (Node n l r)
  | n == x = True
  | n > x  = contains x l
  | otherwise = contains x r
contains _ Leaf = False


t2 :: BST Int
t2 = Node 5 Leaf (Node 20 (Node 10 Leaf Leaf) (Node 30 Leaf Leaf))


-------------------------------------------------------------------------------
-- | In-order traversal (fold)
-------------------------------------------------------------------------------
fold :: (b -> a -> b) -> b -> BST a -> b

fold f b (Node n l r) = fold f (f (fold f b l) n) r
fold _ b Leaf = b

toList :: BST a -> [a]
toList = reverse . fold (\xs x -> x:xs) []

toString :: (Show a) => BST a -> String
toString t = "build " ++ show (toList t) 


-------------------------------------------------------------------------------
-- | Adding an element
-------------------------------------------------------------------------------
add :: (Ord a) => a -> BST a -> BST a
add x (Node n l r)
  | contains x (Node n l r) = Node n l r
  | n >= x = Node n (add x l) r
  | otherwise = Node n l (add x r)
add x Leaf = Node x Leaf Leaf
-------------------------------------------------------------------------------
-- | Removing the minumum element
-------------------------------------------------------------------------------
removeMin :: (Ord a) => BST a -> (a, BST a)

removeMin (Node n Leaf r) = (n, r)
removeMin (Node n l r) = let (minVal, newLeft) = removeMin l in (minVal, Node n newLeft r)
removeMin Leaf = error "Empty tree"

-------------------------------------------------------------------------------
-- | Removing an element
-------------------------------------------------------------------------------
remove :: (Ord a) => a -> BST a -> BST a

remove x (Node n l r)
  | x == a    = remove x b
  | otherwise = add a (remove x b)
  where
    (a, b) = removeMin (Node n l r)
remove x Leaf = Leaf

-------------------------------------------------------------------------------
-- | QuickCheck Properties
-------------------------------------------------------------------------------

--  Holds after `build`
prop_build :: [Int] -> Bool
prop_build xs = isOrdered (build xs)

--  Holds after `contains` and `build`
prop_contains_elt :: Int -> [Int] -> Bool
prop_contains_elt x xs = (x `elem` xs) == (contains x (build xs))

--  Holds after `contains` and `fold`
prop_contains_elts :: BST Int -> Bool 
prop_contains_elts t = and [ contains x t | x <- toList t ] 

-- Holds after `add`
prop_add_elt :: Int -> BST Int -> Bool 
prop_add_elt elt t = contains elt (add elt t) 
  
-- Holds after `add`
prop_add_elts_old :: Int -> BST Int -> Bool 
prop_add_elts_old elt t = forall t (\x -> contains x t') 
  where 
    t'                  = add elt t   

-- Holds after `add`
prop_add_isOrd :: Int -> BST Int -> Bool
prop_add_isOrd elt t = isOrdered (add elt t)

-- Holds after `add`: Fix this property
prop_multiset :: [Int] -> Bool 
prop_multiset xs = toList (build xs) == L.sort (removeDuplicates xs)   -- <<<< TBD: you need to fix this property
  where
    removeDuplicates :: [Int] -> [Int]
    removeDuplicates l = reverse (helper [] l)
      where
        helper :: [Int] -> [Int] -> [Int]
        helper seen []     = seen
        helper seen (x:xs) = helper seen' rest'
          where
            seen'          = if elem x seen == False then [x] ++ seen else seen
            rest'          = xs


-- Holds after `removeMin`

-- Holds after `removeMin`
prop_remove_min :: BST Int -> Bool
prop_remove_min Leaf = True
prop_remove_min t    = contains x t && forall t' (\y -> x < y) 
  where 
    (x, t')          = removeMin t

-- Holds after `remove`
prop_remove :: Int -> BST Int -> Bool 
prop_remove elt t = not (contains elt t') 
  where 
    t'            = remove elt t 

-- Holds after `remove`
prop_remove_old :: Int -> BST Int -> Bool 
prop_remove_old elt t = forall t (\x -> x == elt || contains x t') 
  where 
    t'                = remove elt t 

-- Holds after `remove`
prop_remove_isOrd :: Int -> BST Int -> Bool
prop_remove_isOrd elt t = isOrdered (remove elt t)

-------------------------------------------------------------------------------
-- | QuickCheck Instance
-------------------------------------------------------------------------------
quickCheck :: (QC.Testable prop) => prop -> IO ()
quickCheck = QC.quickCheck

instance (Ord a, QC.Arbitrary a) => QC.Arbitrary (BST a) where
  arbitrary = build <$> QC.arbitrary