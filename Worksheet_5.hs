{- Week5.hs
 This file illustrates list patterns and recursion over lists.
-}

import Prelude hiding (fst, snd, head, tail, sum, concat, reverse, zip)

-- Definitions of the prelude functions fst and snd

fst (x,_)       = x
snd (_,y)       = y

-- Definitions of the prelude functions head and tail

head (x:_)      = x
tail (_:xs)     = xs

absFirst :: [Int] -> Int
absFirst []     = -1
absFirst (x:xs) = abs x

sum :: [Int] -> Int 
sum []     = 0
sum (x:xs) =   x + sum xs

doubleAll :: [Int] -> [Int]
doubleAll []      = []
doubleAll (x:xs)  = 2*x : doubleAll xs

concat :: [[a]] -> [a]
concat []         = []
concat (x:xs)     = x ++ concat xs

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip (x:xs) (y:ys)  = (x,y) : zip xs ys
zip _ _            = []

type StudentMark = (String, Int)

-- List Patterns

--1.

headPlusOne :: [Int] -> Int
headPlusOne x
    | x == [] = 0
    | otherwise = head x +1 

--1.5. With colon

headPlusOneTwo :: [Int] -> Int
headPlusOneTwo (x:xs) = x+1
headPlusOneTwo [] = 0 -- base case
    
--2.

duplicateHead :: Eq a => [a] -> [a]
duplicateHead x
    | x == [] = []
    | otherwise = head x : x
    
--2.5. With colon
duplicateHeadTwo :: [a] -> [a]
duplicateHeadTwo (x:xs) = x:x:xs
duplicateHeadTwo [] = [] -- base case

--3. rotate
rotate :: [a] -> [a]
rotate (x:y:xs) = y:x:xs

--4.

listLength :: [a] -> Int
listLength [] = 0 -- base case
listLength (x:xs) = 1 + listLength (xs)

--5.

multAll :: [Int] -> Int
multAll (x:xs) =  x * multAll xs
multAll [] = 1

--6.

andAll :: [Bool] -> Bool
andAll [] = True
andAll (x:xs) = x && andAll xs

--7.

countElems :: Int -> [Int] -> Int
countElems i [] = 0
countElems i (x:xs) 
    | i == x = 1 + countElems i (xs)
    | otherwise = countElems i (xs)
    
--8.

removeAll :: Int -> [Int] -> [Int]
removeAll i [] = []
removeAll i (x:xs)
    | i == x = removeAll i (xs)
    | otherwise = x : removeAll i (xs)
    
--9.

listMarks :: String -> [StudentMark] -> [Int]
listMarks name [] = []
listMarks name ((x,y):xs) 
    | name == x = y : listMarks name (xs)
    | otherwise = listMarks name (xs)
    
--10.
prefix :: [Int] -> [Int] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
    | x == y = prefix (xs) (ys)
    | otherwise = False 
    
--11.
subSequence :: [Int] -> [Int] -> Bool
subSequence [] _ = True
subSequence _ [] = False
subSequence (x:xs) (y:ys)
    | x /= y = subSequence (x:xs) (ys)
    | x == y = subSequence (xs) (ys)
    | otherwise = False