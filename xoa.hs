-- Viết hàm xoa trong Haskell nhập vào n và một list as. Cứ i-th phần tử thì xóa 1 phần tử khỏi list as, với i từ 1 đến n. Phần tử đầu tiên bắt đầu từ 1.
-- Ví dụ: xoa 5 [1..20] = [1 3 4 6 7 8 10 11 12 13 15 16 17 18 19]

indexList :: Int -> Int -> [Int]
indexList a b = [a] ++ indexList (a+b) (b+1)
-- This can be implemented by ... = a : indexList (a+b) (b+1)

xoa :: Int -> [a] -> [a]
xoa n as = [x | (x, y) <- zip as [1..], elem y (take n (indexList 2 3)) == False]
