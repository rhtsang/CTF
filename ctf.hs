import Data.List
import Data.Char

boardSize board = (round (sqrt (fromIntegral (length board))))

genMoves :: [Char] -> [[Char]] -> [Int]-> [[Char]] -> [[Char]]
genMoves board history indices moves
 | indices == [] = filter (\n -> not (elem n history)) (filter (not . null) moves)
 | otherwise = genMoves board history (tail indices) (moves++(genMovesHelper board indices []))

genMovesHelper :: [Char] -> [Int] -> [[Char]] -> [[Char]]
genMovesHelper board indices moves
 | indices == [] = moves
 | otherwise = moves++[move1Up board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move1Down board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move1Left board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move1Right board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move2Up board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move2Down board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move2Left board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]++[move2Right board (board!!(head indices)) (head indices) (round (sqrt (fromIntegral (length board))))]


move1Up :: [Char] -> Char -> Int->  Int -> [Char]
move1Up currBoard piece index boardSize
 | index-boardSize >= 0 && currBoard!!(index-boardSize) == '-' = replaceNth index '-' (replaceNth (index-boardSize) piece currBoard)
 | otherwise = []

move1Down :: [Char] -> Char -> Int->  Int -> [Char]
move1Down currBoard piece index boardSize
 | index+boardSize < boardSize^2 && currBoard!!(index+boardSize) == '-' = replaceNth index '-' (replaceNth (index+boardSize) piece currBoard)
 | otherwise = []

move1Left :: [Char] -> Char -> Int->  Int -> [Char]
move1Left currBoard piece index boardSize
 | index-1 >= 0 && index `mod` boardSize /= 0 && currBoard!!(index-1) == '-' = replaceNth index '-' (replaceNth (index-1) piece currBoard)
 | otherwise = []

move1Right :: [Char] -> Char -> Int->  Int -> [Char]
move1Right currBoard piece index boardSize
 | index+1 < boardSize^2 && (index+1) `mod` boardSize /= 0 && currBoard!!(index+1) == '-' = replaceNth index '-' (replaceNth (index+1) piece currBoard)
 | otherwise = []

move2Up :: [Char] -> Char -> Int->  Int -> [Char]
move2Up currBoard piece index boardSize
 | piece == 'b' && index-(2*boardSize) >= 0 && currBoard!!(index-(2*boardSize)) == '-' && (currBoard!!(index-boardSize) == 'w' || currBoard!!(index-boardSize) == 'W') = replaceNth (index-(2*boardSize)) piece (replaceNth index '-' (replaceNth (index-boardSize) '-' currBoard))
 | otherwise = []

move2Down :: [Char] -> Char -> Int->  Int -> [Char]
move2Down currBoard piece index boardSize
 | piece == 'w' && index+(2*boardSize) < boardSize^2 && currBoard!!(index+(2*boardSize)) == '-' && (currBoard!!(index+boardSize) == 'b' || currBoard!!(index+boardSize) == 'B') = replaceNth (index+(2*boardSize)) piece (replaceNth index '-' (replaceNth (index+boardSize) '-' currBoard))
 | otherwise = []

move2Left :: [Char] -> Char -> Int->  Int -> [Char]
move2Left currBoard piece index boardSize
 | piece == 'b' && index-2 >= 0 && (index-1) `mod` boardSize /= 0 && index `mod` boardSize /= 0 && currBoard!!(index-2) == '-' && (currBoard!!(index-1) == 'w' || currBoard!!(index-1) == 'W') = replaceNth (index-2) piece (replaceNth index '-' (replaceNth (index-1) '-' currBoard))
 | piece == 'w' && index-2 >= 0 && (index-1) `mod` boardSize /= 0 && index `mod` boardSize /= 0 && currBoard!!(index-2) == '-' && (currBoard!!(index-1) == 'b' || currBoard!!(index-1) == 'B') = replaceNth (index-2) piece (replaceNth index '-' (replaceNth (index-1) '-' currBoard))
 | otherwise = []

move2Right :: [Char] -> Char -> Int->  Int -> [Char]
move2Right currBoard piece index boardSize
 | piece == 'b' && index+2 < boardSize^2 && (index+1) `mod` boardSize /= 0 && (index+2) `mod` boardSize /= 0 && currBoard!!(index+2) == '-' && (currBoard!!(index+1) == 'w' || currBoard!!(index+1) == 'W') = replaceNth (index+2) piece (replaceNth index '-' (replaceNth (index+1) '-' currBoard))
 | piece == 'w' && index+2 < boardSize^2 && (index+1) `mod` boardSize /= 0 && (index+2) `mod` boardSize /= 0 && currBoard!!(index+2) == '-' && (currBoard!!(index+1) == 'b' || currBoard!!(index+1) == 'B') = replaceNth (index+2) piece (replaceNth index '-' (replaceNth (index+1) '-' currBoard))
 | otherwise = []

printAllBoards :: [[Char]] -> IO()
printAllBoards boards = putStr (printAllBoardsHelper boards [])

printAllBoardsHelper :: [[Char]] -> [Char] -> [Char]
printAllBoardsHelper boards print
 | boards == [] = print
 | otherwise = printAllBoardsHelper (tail boards) ((print ++ printBoardHelper (head boards) (boardSize (head boards)) 1 []))


--printBoard :: [Char] -> Int -> [Char] -> IO()
--printBoard board boardSize print = putStr (printBoardHelper board boardSize 1 [])

printBoardHelper :: [Char] -> Int -> Int -> [Char] -> [Char]
printBoardHelper board bSize column print
 | board == "" = "\n" ++ print
 | column == bSize = print ++ [(head board)] ++ " " ++ "\n" ++ printBoardHelper (tail board) bSize 1 print
 | otherwise = print ++ [(head board)] ++ " " ++ (printBoardHelper (tail board) bSize (column+1) print)

replaceNth :: Int -> Char -> [Char] -> [Char]
replaceNth index new (x:xs)
 | index == 0 = new:xs
 | otherwise = x:replaceNth (index-1) new xs
