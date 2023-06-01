-- Hàm ztn tính số lần xuất hiện của số nguyên trong 1 danh sách

ztn :: ([Int], Int) -> Int
ztn ([], n) = 0
ztn ((x:xs), n) = ztn (xs, n) + if (x == n) then 1 else 0