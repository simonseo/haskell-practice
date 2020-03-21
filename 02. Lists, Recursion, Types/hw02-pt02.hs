--hellohs my first haskell file!
import Data.List


-- Stack has been installed to: /usr/local/bin/stack

-- NOTE: You may need to run 'xcode-select --install' and/or
--       'open /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg'
--       to set up the Xcode command-line tools, which Stack uses.

-- 2.a. eratoSieve
eratoSieve :: Int -> [Int] -> [Int]
eratoSieve m intList = 
    if length intList > 0
    then
        if (el /= m) && ((mod el m) == 0)
        then rest
        else el:rest
    else []
    where 
        el = head intList
        rest = eratoSieve m (tail intList)

-- 2.b. listOfPrimes
eratoSeiveRange :: Int -> Int -> [Int] -> [Int]
eratoSeiveRange a b intList = 
    if ((length intList) > 0) && (a > 1) && (a < b)
        then eratoSeiveRange (a + 1) b sieved
    else intList
    where sieved = eratoSieve a intList 

listOfPrimes :: Int -> [Int]
listOfPrimes maxN = if maxN > 1 
    then eratoSeiveRange 3 maxFactor mostlyOddNumbers
    else []
    where 
        maxFactor = (round . sqrt . fromIntegral) maxN
        mostlyOddNumbers = 2:[3, 5 .. maxN ]

-- 2.c. listOfPrimesPowerOfTwoMinusOne
-- 2^k - 1
potmo k = (2 ^ k) - 1 

listOfPowerofTwoMinusOne:: Int -> [Int]
listOfPowerofTwoMinusOne k = 
    (if k > 1 then listOfPowerofTwoMinusOne (k - 1) else []) 
    ++ [potmo k]

listOfPrimesPowerOfTwoMinusOne :: Int -> [Int]
listOfPrimesPowerOfTwoMinusOne n = eratoSeiveRange 2 maxFactor (listOfPowerofTwoMinusOne maxExp)
    where
        maxFactor = (potmo . round . sqrt . fromIntegral) n
        maxExp = (floor . logBase 2 . fromIntegral) (1 + n)

-- 2.d. decompInPrimes
smallestFactor :: [Int] -> Int -> Int
smallestFactor candidates n = 
    if (length candidates) > 0
    then 
        if divisible then first else smallestFactor rest n
    else 1 
    -- what should this be? not divisible by any candidate means either not factorizable or wrong candidate list
    where 
        divisible = (mod n first) == 0
        first = head candidates
        rest = tail candidates

decompInPrimes' :: [Int] -> Int -> [Int]
decompInPrimes' candidates n =
    if (n > 1) && (factor > 1)
    then  factor : (decompInPrimes' newcandidates quotient)
    else []
    where 
        factor = smallestFactor candidates n
        quotient = n `div` factor
        newcandidates = if factor > (head candidates) then tail candidates else candidates

decompInPrimes :: Int -> [Int]
decompInPrimes n = decompInPrimes' candidates n
    where candidates = listOfPrimes n -- so that we calculate listOfPrimes only once

-- Main
main = do
    print "hello world!"
    print (eratoSieve 2 [1,2,3])
