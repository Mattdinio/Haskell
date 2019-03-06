import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stMarks = [ mk | (st,mk) <- stMarks ]

pass :: [StudentMark] -> [String]
pass stMarks = [ st | (st,mk) <- stMarks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

--TUPLES

-- 1.
sumDifference :: Int -> Int -> (Int, Int)
sumDifference x y = (x+y, x-y)

--2.
grade :: StudentMark -> Char
grade (s1,m1)
    | m1 >= 70 = 'A'
    | m1 >= 60 = 'B'
    | m1 >= 50 = 'C'
    | m1 >= 40 = 'D'
    | otherwise = 'F'
--3.
capMark :: StudentMark -> StudentMark
capMark (s1,m1)
    | m1 > 40 = (s1,40)
    | otherwise = (s1,m1)
    
-- Lists and Strings

--4.
firstNumbers :: Int -> [Int]
firstNumbers x 
    | x > 0 = [1..x] -- as type is int check value is positive 
    | otherwise = [0]  --return a list containing 0 if 0 or below

--5.
firstSquares :: Int -> [Int]
firstSquares x
    | x > 0 = [i^2 | i <- [1..x]]
    | otherwise = [0]
    
--6.

capitalise :: String ->String
capitalise x = [