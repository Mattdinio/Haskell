getStatusMessage :: Int -> [[Char]]
getStatusMessage value 
    | value == 1 || value == 2 || value == 3 = ["OK"]
    | value == 10 = ["out of disk space"]
    | otherwise = ["out of scope"]