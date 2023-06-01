-- Viết hàm isPalindrome :: [Int] -> Bool kiểm tra xem một mảng có phải mảng đối xứng hay không.
-- Ví dụ: isPalindrome [1,2,3,3,2,1] = True
--        isPalindrome [2,3,3,2,1] = False

isPalindrome :: [Int] -> Bool
isPalindrome [] = True
isPalindrome (xs) = if (xs == reverse xs) then True else False