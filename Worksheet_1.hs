-- 1.
timesTen :: Int -> Int
timesTen x =10*x
-- 2.
sumTree :: Int -> Int -> Int -> Int
sumTree x y z = x+y+z
-- 3.
areaOfCircle :: Float -> Float
areaOfCircle r = pi * r^2
-- 4.
volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder l r = pi * r^2 * l
-- 5.
distance :: Float -> Float -> Float -> Float -> Float
distance x1 x2 y1 y2 = sqrt((y1 - y2)^2 + (x1 - x2)^2)
-- 6.
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = if x == y || x == z || z == y then False else True
-- 7.
divisibleBy :: Int -> Int -> Bool
divisibleBy x y = if  x `mod` y == 0 then True else False
-- 8.
isEven :: Int -> Bool
isEven x = if divisibleBy x 2 == True then True else False
-- 9.
averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral(x+y+z)/3
-- 10.
absolute :: Int -> Int
absolute x = if x > 0 then x else x*(-1)