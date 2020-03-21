import Data.List

-- 1a.
dual :: Char -> Char
dual base = case base of
    'G' -> 'C'
    'C' -> 'G'
    'A' -> 'T'
    'T' -> 'A'
    _ -> base

dualStrandSeq :: String -> String
dualStrandSeq seq = if length seq > 0 
    then (dual (head seq)) : (dualStrandSeq (tail seq)) 
    else ""

-- 1b.
isPrefix :: String -> String -> Bool
isPrefix str1 str2 = str1 == take l str2
    where l = length str1

-- 1c.
isSubString :: String -> String -> Bool
isSubString str1 str2 = if (length str2 == 0) && (length str1 > 0)
    then False
    else isPrefix str1 str2 || isSubString str1 (tail str2)

-- 1d.
isSubSeq :: String -> String -> Bool
isSubSeq str1 str2 = 
    if length str1 == 0 then True 
    else if length str2 == 0 then False 
    else 
        if (head str1) == (head str2)
        then isSubSeq (tail str1) (tail str2) 
        else isSubSeq str1 (tail str2)

-- 1e. listOfSubString
prefixes :: [a] -> [[a]]
prefixes str = 
    if l == 0 then []
    else str : prefixes left
    where 
        l = length str
        left = take (l-1) str 

listOfSubString :: [a] -> [[a]]
listOfSubString str = 
    if length str == 0 then [[]] -- include empty string
    else (prefixes str) ++ listOfSubString (tail str)


-- 1f. listOfSubSeq
prependToAll :: a -> [[a]] -> [[a]]
prependToAll c lstOfStr = 
    if length lstOfStr == 0 then [] -- [[]] gives duplicate values
    else prependedHead : prependedTail
    where 
        prependedHead = c : (head lstOfStr)
        prependedTail = prependToAll c (tail lstOfStr)

listOfSubSeq :: [a] -> [[a]]
listOfSubSeq str = 
    if length str == 0 then [[]]
    else subSeqsWithoutHead ++ subSeqsWithHead
    where 
        subSeqsWithoutHead = listOfSubSeq (tail str)
        subSeqsWithHead = prependToAll (head str) subSeqsWithoutHead

-- 1g. Embeddings
indexOf :: (Eq a) => [a] -> a -> Int
indexOf str c = 
    if notExists then -1
    else if isFirstElement then 1
    else 1 + indexInTail
    where 
        notExists = not $ elem c str
        isFirstElement = c == (head str)
        indexInTail = indexOf (tail str)  c

-- allIndices offset c str = all indices of character in string starting from 1+offset 
-- allIndices 'A' "GAACAT" = [2,3,5]
allIndices :: (Eq a) => Int -> [a] -> a -> [Int]
allIndices offset str c =
    if notExists then []
    else  firstIndex:indicesInRest
    where
        i = indexOf str c
        notExists = i < 0
        firstIndex = offset+i
        indicesInRest = allIndices firstIndex (drop i str) c

-- listOfAllIndices str1 str2 = all indices of [each character in str1] in str2
-- listOfAllIndices "GACA" "GAACAT" = [[1],[2,3,5],[4],[2,3,5]]
listOfAllIndices :: (Eq a) => [a] -> [a] -> [[Int]]
listOfAllIndices str1 str2 = map positionInStr2 str1 
    where positionInStr2 = allIndices 0 str2

prependIfIncreasing :: (Ord a) => a -> [[a]] -> [[a]]
prependIfIncreasing n listOfLists = 
    if length listOfLists == 0 then []
    else 
        if isInIncreasingOrder 
        then (n : list) : rest
        else rest
    where 
        list = head listOfLists
        isInIncreasingOrder = (length list == 0) || (n < (head list))
        rest = prependIfIncreasing n (tail listOfLists)

-- [1,2] x [[2,3], [3,4]] -> [[1,2,3],[1,3,4],[2,3,4]]
_permute :: (Ord a) => [a] -> [[a]] -> [[a]]
_permute list listOfLists =
    if (length list == 0) || (length listOfLists == 0) then []
    else (prependIfIncreasing (head list) listOfLists) ++ rest
    where
        rest =  _permute (tail list) listOfLists

-- [[1,2], [2,3], [3,4]] -> [[1,2,3],[1,3,4],[2,3,4]]
permute :: (Ord a) => [[a]] -> [[a]]
permute listOfLists =
    if l == 0 then []
    else _permute (head listOfLists) tailPermutation
    where
        l = length listOfLists
        tailPermutation = if l > 1 then permute (tail listOfLists) else [[]]

-- listOfEmbed "ACA" "AACAT" = [[1,3,4],[2,3,4]]
listOfEmbed :: (Eq a) => [a] -> [a] -> [[Int]]
listOfEmbed str1 str2 = permute $ listOfAllIndices str1 str2
 
-- Main
main = do
    print "hello world!"
    print $ dualStrandSeq "GAGAGTCATXXX"
    print $ isPrefix "AA" "AACAA"
    print $ isSubString "CAT" "GAACATA"
    print $ isSubSeq "GAAATA" "GAACATA"

    print $ sort $ listOfSubString "123"
    print $ sort $ listOfSubSeq "123"

    print $ listOfAllIndices "GACA" "GAACAT"
    print $ permute [[1],[2,3,5],[4],[2,3,5]]
    print $ listOfEmbed "GACA" "GAACAT"

