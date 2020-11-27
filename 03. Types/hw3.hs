import Data.Char (isLetter, toLower)
-- 1. Palindrome
-- 1a. reverseList

reverseList :: [a] -> [a]
reverseList (x:[]) = [x]
reverseList xs = (last xs) : reverseList (init xs)


-- 1b. isPalindrome
isPalindrome :: String -> Bool
isPalindrome xs = xs == reverseList xs


-- 1c. isPalindromeStr
normalizeStr :: String -> String
normalizeStr [] = []
normalizeStr (x:xs) = if isLetter x then (toLower x):rest else rest
    where 
        rest = normalizeStr xs

isPalindromeStr :: String -> Bool
isPalindromeStr = isPalindrome . normalizeStr 

-- 1d. 
-- isPalindromeStr is not of the general type (Eq a) => [a] -> Bool ,
-- which isPalindrome is, because isPalindromeStr uses isLetter and toLower,
-- which are Char functions and cannot be applied to general Eq data types.

-- 2a.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:[]) = [x]
quicksort (x:xs) = lowerValues ++ [x] ++ greaterValues
    where
        lowerValues = quicksort $ filter (<= x) xs  
        greaterValues = quicksort $ filter (x <) xs

-- 2b.
-- quicksort :: [Int] -> [Int] is also of the type quicksort :: (Ord a) => [a] -> [a]
-- because the only functions we use on `a` internally is the comparison functions (lt/gt)
-- which can apply to Ord class variables in general

-- 3a. Create a data constructor and use it
-- is `Tree` the data constructor? or this entire expression?
data Tree = Nil | Node Int Tree Tree

instance Eq Tree where
    Nil == Nil = True
    (Node n1 l1 r1) == (Node n2 l2 r2) = (n1 == n2 && l1 == l2 && r1 == r2)
    _ == _ = False

illustration :: Tree
illustration = Node 2 (Node 1 Nil Nil) (Node 6 (Node 4 (Node 3 Nil Nil) (Node 5 Nil Nil)) (Node 7 Nil Nil))

-- left :: Tree -> Tree
-- left Nil = Nil
-- left (Node _ tree _) = tree 

-- right :: Tree -> Tree
-- right Nil = Nil
-- right (Node _ _ tree) = tree

-- value :: Node -> Int
-- value (Node num _ _) = num

-- 3b. in-order treeTraversal 
treeTraversal :: Tree -> [Int]
treeTraversal Nil = []
treeTraversal (Node value left right) = (treeTraversal left) ++ [value] ++ (treeTraversal right)


-- 3c. treeLeaves
treeLeaves :: Tree -> [Int]
treeLeaves Nil = []
treeLeaves (Node value left right) = (treeLeaves left) ++ val ++ (treeLeaves right)
    where
        val = if isLeaf then [value] else []
        isLeaf = left == Nil && right == Nil

-- 3d. showTree
showTree :: Tree -> String
showTree Nil = ""
showTree (Node value left right) = "(" ++ (showTree left) ++ ")" ++ (show value) ++ "(" ++ (showTree right) ++ ")"

-- 3e. Show
instance Show Tree where
    show Nil = ""
    show (Node value left right) = "(" ++ (show left) ++ ")" ++ (show value) ++ "(" ++ (show right) ++ ")"


-- 3f. sumToBranch
sumToBranch :: Tree -> Tree
sumToBranch Nil = Nil
sumToBranch (Node v l r) = Node v (sumToBranch $ addToRoot l) (sumToBranch $ addToRoot r)
    where 
        addToRoot Nil = Nil
        addToRoot (Node val l r) = Node (val+v) l r 

-- 4a. Generic binary tree
data BinTree a = Empty | BinNode a (BinTree a) (BinTree a)

treeToBinTree :: Tree -> BinTree Int
treeToBinTree Nil = Empty
treeToBinTree (Node v l r) = BinNode v (treeToBinTree l) (treeToBinTree r)

binillustration = treeToBinTree illustration

-- 4b. showBinTree
showBinTree :: (Show a) => BinTree a -> String
showBinTree Empty = ""
showBinTree (BinNode v l r) = "(" ++ (showBinTree l) ++ ")" ++ (show v) ++ "(" ++ (showBinTree r) ++ ")"

-- 4c. Show
instance (Show a) => Show (BinTree a) where
    show Empty = ""
    show (BinNode v l r) = "(" ++ (show l) ++ ")" ++ (show v) ++ "(" ++ (show r) ++ ")"

-- 4d. listToBranch
listToBranch' :: BinTree [a] -> BinTree [a]
listToBranch' Empty = Empty
listToBranch' (BinNode v l r) = BinNode v (listToBranch' $ appendValue l) (listToBranch' $ appendValue r)
    where
        appendValue Empty = Empty
        appendValue (BinNode val l r) = BinNode (v++val) l r

listToBranch :: BinTree a -> BinTree [a]
listToBranch = listToBranch' . convert
    where
        convert Empty = Empty
        convert (BinNode v l r) = BinNode [v] (convert l) (convert r)



main = do
    print $ reverseList "Hello"
    print $ isPalindrome "racecar"
    print $ isPalindromeStr "Madam I'm Adam"
    print $ quicksort [45,245,7,2,42,228,2,5]
    print $ treeTraversal illustration
    print $ treeLeaves illustration
    print illustration
    print $ sumToBranch illustration
    print binillustration
    print $ listToBranch binillustration