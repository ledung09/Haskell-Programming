-- There are some ways to define the function:
-- perfects :: Int -> [Int]
-- For example: perfects 500 will return [6,28,496]. Just follow the knowledges in the courses
-- Perfect number definition: A number equal to sum of all of its divisors (except itself).

isPerfect :: Int -> Bool
isPerfect x = if (x == sum ([i | i <- [1..x-1], x `mod` i == 0])) then True else False

perfects :: Int -> [Int]
perfects x = [i |i <- [1..x], (isPerfect i) == True]