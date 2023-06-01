-- Sử dụng kiến thức về viết script được cung cấp trong khóa học để viết hàm 
-- Ví dụ: sum_square [1..10] sẽ trả về kết quả 1 + 4 + 9 == 14

isSquare :: Int -> Bool
isSquare x = if (x < 0) then False else (elem x [i^2 | i <- [0..x]])

sum_square :: [Int] -> Int
sum_square [] = 0
sum_square (x:xs) = (sum_square xs) + if ((isSquare x) == True) then x else 0; 