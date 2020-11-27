
-- 1. Exercise 1 – implement a generator of binary trees
data Tree = Leaf | Node Tree Tree deriving (Show)

-- 1.a. countNode
countNode :: Tree -> Int
countNode Leaf = 0
countNode (Node left right) = 1 + countNode left + countNode right

-- 1.b. genTree generates all the binary trees of a given size
genTree :: Int -> [Tree]

main = do
    print $ countNode (Node Leaf Leaf)
    print $ countNode (Node Leaf Leaf)
