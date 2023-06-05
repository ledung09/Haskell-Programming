-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


{-
Problem 8: Eliminate consecutive duplicates of list elements. If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example: λ> compress "aaaabccaadeeee"
         "abcade"
-}

-- Sol1: Zip an element to its next element, if same pair -> ignore, else append first element.
zipConsecutive :: [a] -> [(a,a)]
zipConsecutive xs = zip (init xs) (tail xs)

compress :: (Eq a) => [a] -> [a]
compress xs = [x|(x,y) <- (zipConsecutive xs), x /= y] ++ [last xs]

-- Sol2: Util function, compare each element with its head, if the same dont care, else append to the list.
compressUtil :: (Eq a) => a -> [a] -> [a]
compressUtil _ [] = []
compressUtil y (x:xs) = if (y == x) then (compressUtil y xs) else [x] ++ compressUtil x xs

compress' :: (Eq a) => [a] -> [a]
compress' xs = [head xs] ++ compressUtil (head xs) xs


{- 
Problem 14: Duplicate the elements of a list.
Example: λ> dupli [1, 2, 3]
         [1,1,2,2,3,3]
-}

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs


{-
Problem 15: Replicate the elements of a list a given number of times.
Example: λ> repli "abc" 3
         "aaabbbccc"
-}

mul :: a -> Int -> [a]
mul _ 0 = []
mul x n = [x] ++ (mul x (n-1))

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (mul x n) ++ (repli xs n)


{- 
Problem 16: Drop every N'th element from a list.
Example: λ> dropEvery "abcdefghik" 3
         "abdeghk"
-}

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x|(x,y) <- zip xs [1..], y `mod` n /= 0]


{- 
Problem 18: Extract a slice from a list.
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
Example: λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
         "cdefg"
-}

slice :: [a] -> Int -> Int -> [a]
slice xs i k = [x|(x,y) <- zip xs [1..], i <= y, y <= k]


{- 
Problem 19: Rotate a list N places to the left.
Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

Example 1: λ> rotate ['a','b','c','d','e','f','g','h'] 3
           "defghabc"

Example 2: λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
           "ghabcdef"
-}

rotate :: [a] -> Int -> [a]
rotate xs n = if (n == 0) then xs else if (n > 0) then (rotate (tail xs ++ [head xs]) (n-1)) else (rotate ([last xs] ++ init xs) (n+1))