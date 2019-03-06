--Worksheet 2: Introduction to Functional Programming II

-- 1. Absolute 
absolute :: Int -> Int
absolute x 
    | x>0 = x
    | otherwise = x*(-1)
    
--2. sign
sign :: Int -> Int
sign x
    | x>0 = 1
    | x<0 = (-1)
    | otherwise = 0

--3. how many are equal?
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | otherwise = 0
    
--4. sum of diagonal lengths of squares
sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = a*x + a*y + a*z
    where 
    a = sqrt 2
    
--5. taxi fare calculator
taxiFare :: Int -> Float
taxiFare km
    | km > 0 &&  km < 10 = initialFare + fromIntegral(km)*0.5
    | km > 10  = initialFare + firstTenkm + fromIntegral(km-10)*0.3
    | otherwise = initialFare
    where
    initialFare = 2.20
    firstTenkm = 10 * 0.5

--6. how many above the average sum of all 3 numbers
howManyAverage :: Int -> Int -> Int -> Int
howManyAverage x y z
    | xGreater && (yGreater || zGreater) = 2 -- if X and y or z are greater than 2 must be
    | yGreater && zGreater = 2 -- if y and z are greater than it must be 2 
    | xGreater || yGreater || zGreater = 1 -- if anything is bigger than it's the only one at this stage
    | otherwise = 0 --If all 3 numbers are the same E.g 5,5,5 then the average is 5 and the same but not greater than the average
    where
    averageValue = (x+y+z) `div` 3 
    xGreater = x>averageValue 
    yGreater = y>averageValue 
    zGreater = z>averageValue 
    
--7. valid date checker [Simple solution without lists]
validDate :: Int -> Int -> Bool
validDate day month
    | day > 31 || day <= 0 || month > 12 || month <= 0 = False -- checks that the day and month are valid E.g Day 1..31 Month 1..12
    | month == 2 && day > 28 = False -- checks that is the month if February that the days are not more than 28
    | day == 31 && thirtyDayMonth = False
    | otherwise = True
    where
    thirtyDayMonth = month == 4 || month == 6 || month == 9 || month == 11
    
--8.  Days in month 
daysInMonth :: Int -> Int -> Int
daysInMonth month year
    | month `elem` [4,6,9,11] = 30
    | month `elem` [1,3,5,7,8,10,12] = 31
    | year `mod` 4 == 0 = 29
    | otherwise = 28

--7.5 valid date checker [advanced solution with lists]

validDate2 :: Int -> Int -> Bool
validDate2 day month
    | day > 31 || day <= 0 || month > 12 || month <= 0 = False -- checks that the day and month are valid E.g Day is between 1..31 and Month 1..12
    | month == 2 && day > 28 = False -- checks that is the month if February that the days are not more than 28
    | day == 31 && month `elem` [4,6,9,11]  = False -- if day is 31 is the month April, June, September or November
    | otherwise = True