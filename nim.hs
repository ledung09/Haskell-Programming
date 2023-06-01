-- Implement the game of Nim

-- Initialize the board
board :: [Int]
board = [5,4,3,2,1]

-- Print board
printBoardLn :: Int -> IO ()
printBoardLn 0 = putStr ""
printBoardLn x = do
 putStr "*"
 printBoardLn (x-1)

printBoard :: [Int] -> Int -> IO ()
printBoard _ 6 = putStrLn ""
printBoard (x:xs) idx = do
 putStr ((show idx) ++ ":")
 printBoardLn x
 putStrLn ""
 printBoard xs (idx+1)
  
-- Update board
update :: [Int] -> Int -> Int -> [Int]
update oldBoard row star = [if (y == row) then x-star else x|(x,y) <- zip oldBoard [1..5]]

-- Game play
play :: [Int] -> Int -> IO()
play curBoard turn = do
 putStrLn ("Player " ++ (show ((turn `mod` 2) + 1)) ++ " is playing …")
 putStr "Row? "
 input1 <- readLn
 putStr "How many stars? "
 input2 <- readLn
 printBoard (update curBoard input1 input2) 1
 play (update curBoard input1 input2) (turn+1)
 
-- Main function
main :: IO()
main = do
 putStrLn "Initializing …" 
 printBoard board 1
 play board 0
 
