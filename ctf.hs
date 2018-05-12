import Data.List
import Data.Char

{-
3x3
0 1 2
3 4 5
6 7 8

4x4
 0  1  2  3
 4  5  6  7
 8  9 10 11
12 13 14 15

5x5
 0  1  2  3  4
 5  6  7  8  9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24

how to get all 'w's
elemIndices 'w' "---ww---"
-}

testallw board = printAllBoards (genMoves board [] (elemIndices 'w' board) [])
test1 = printAllBoards (genMoves "-wWw--www-------bbb--bBb-" [] [6,7] [])
test2 = printAllBoards (genMoves "-wWw-w-ww-------bbb--bBb-" ["-wWw--www-------bbb--bBb-"] [6,7] [])
test3 = printAllBoards (genMoves "-Ww---bB-" [] [1, 2] [])
test4 = printAllBoards (genMoves "-W---b-Bw" [] [1, 8] [])
test5 = map evalWhitePieces (genMoves "bW---b-Bw" [] [0, 8] [])
test5p = printAllBoards (genMoves "bW---b-Bw" [] [0, 8] [])
test6 = testallw "-wWw--www-------bbb--bBb-"
test7 = testallw "--W--ww-b-b--B--"

-- | Static eval of board for white by counting pieces. Kings 10, pawns 1.
evalWhitePieces :: [Char] -> Int
evalWhitePieces [] = 0
evalWhitePieces (x:xs)
  | x == 'w'  = 1 + evalWhitePieces xs
  | x == 'b'  = -1 + evalWhitePieces xs
  | x == 'W'  = 10 + evalWhitePieces xs
  | x == 'B'  = -10 + evalWhitePieces xs
  | otherwise = evalWhitePieces xs

-- | Counts the number occurances of x in list
countOccurance :: Char -> [Char] -> Int
countOccurance x list = (length . filter (==x)) list

-- | Calculates the dimension of the board
boardSize board = (round (sqrt (fromIntegral (length board))))

-- | Given a board and a history of moves and a list of pieces (indices)
-- | generates all possible moves for the given pieces
genMoves :: [Char] -> [[Char]] -> [Int]-> [[Char]] -> [[Char]]
genMoves board history [] moves = filter (\n -> not (elem n history)) (filter (not . null) moves)
genMoves board history (i:indices) moves = genMoves board history indices
                                       (moves++(genMovesHelper board (boardSize board) i))

-- | Creates all the possible moves for a single piece i
genMovesHelper :: [Char] -> Int -> Int -> [[Char]]
genMovesHelper board size i =
    [move1Up    board size i]++
    [move1Down  board size i]++
    [move1Left  board size i]++
    [move1Right board size i]++
    [move2Up    board size i]++
    [move2Down  board size i]++
    [move2Left  board size i]++
    [move2Right board size i]

-- | white pawns cant go up
move1Up :: [Char] -> Int->  Int -> [Char]
move1Up board size i
 | i-size >= 0 &&
   board!!(i-size) == '-' &&
   board!!i /= 'w'
    = replaceNth i '-' (replaceNth (i-size) (board!!i) board)
 | otherwise = []

-- | black pawns cant go down
move1Down :: [Char] -> Int->  Int -> [Char]
move1Down board size i
 | i+size < size^2 &&
   board!!(i+size) == '-' &&
   board!!i /= 'b'
    = replaceNth i '-' (replaceNth (i+size) (board!!i) board)
 | otherwise = []

move1Left :: [Char] -> Int->  Int -> [Char]
move1Left board size i
 | i-1 >= 0 &&
   i `mod` size /= 0 &&
   board!!(i-1) == '-'
    = replaceNth i '-' (replaceNth (i-1) (board!!i) board)
 | otherwise = []

move1Right :: [Char] -> Int->  Int -> [Char]
move1Right board  size i
 | i+1 < size^2 &&
   (i+1) `mod` size /= 0 &&
   board!!(i+1) == '-'
    = replaceNth i '-' (replaceNth (i+1) (board!!i) board)
 | otherwise = []

-- | Only black pawns can move 2 up and they have to jump over w or W
move2Up :: [Char] -> Int->  Int -> [Char]
move2Up board  size i
 | (board!!i) == 'b' &&
   i-(2*size) >= 0 &&
   board!!(i-(2*size)) == '-' &&
   (board!!(i-size) == 'w' || board!!(i-size) == 'W')
    = replaceNth (i-(2*size)) (board!!i) (replaceNth i '-' (replaceNth (i-size) '-' board))
 | otherwise = []

-- | Only white pawns can move 2 down and they have to jump over b or B
move2Down :: [Char] -> Int->  Int -> [Char]
move2Down board  size i
 | (board!!i) == 'w' &&
   i+(2*size) < size^2 &&
   board!!(i+(2*size)) == '-' &&
   (board!!(i+size) == 'b' || board!!(i+size) == 'B')
   = replaceNth (i+(2*size)) (board!!i) (replaceNth i '-' (replaceNth (i+size) '-' board))
 | otherwise = []

-- | Checks that range and that b is jumping or w/W or w over b/B
move2Left :: [Char] -> Int->  Int -> [Char]
move2Left board  size i
 | i-2 >= 0 &&
   (i-1) `mod` size /= 0 &&
   i `mod` size /= 0 &&
   board!!(i-2) == '-' &&
   ((board!!i) == 'b' && (board!!(i-1) == 'w' || board!!(i-1) == 'W') ||
   (board!!i) == 'w' && (board!!(i-1) == 'b' || board!!(i-1) == 'B'))
      = replaceNth (i-2) (board!!i) (replaceNth i '-' (replaceNth (i-1) '-' board))
 | otherwise = []

-- | Checks that range and that b is jumping or w/W or w over b/B
move2Right :: [Char] -> Int->  Int -> [Char]
move2Right board size i
 | i+2 < size^2 &&
   (i+1) `mod` size /= 0 &&
   (i+2) `mod` size /= 0 &&
   board!!(i+2) == '-' &&
   ((board!!i) == 'b' && (board!!(i+1) == 'w' || board!!(i+1) == 'W') ||
   (board!!i) == 'w' && (board!!(i+1) == 'b' || board!!(i+1) == 'B'))
      = replaceNth (i+2) (board!!i) (replaceNth i '-' (replaceNth (i+1) '-' board))
 | otherwise = []

-- | Takes a list of board strings and prints out each one as a square board
printAllBoards :: [[Char]] -> IO()
printAllBoards boards = putStr (printAllBoardsHelper boards [])

-- | Tail Recursive helper. Stores the output in 'print'
-- | Goes through every board string and calls printBoardHelper for it
printAllBoardsHelper :: [[Char]] -> [Char] -> [Char]
printAllBoardsHelper boards print
 | boards == [] = print
 | otherwise = printAllBoardsHelper (tail boards) ((print ++ printBoardHelper (head boards) (boardSize (head boards)) 1 []))

-- | Not used
printBoard :: [Char] -> IO()
printBoard board = putStr (printBoardHelper board (boardSize board) 1 [])

-- | Tail Recursive helper. Stores the output in 'print'
-- | Breaks down a board string using board size.
printBoardHelper :: [Char] -> Int -> Int -> [Char] -> [Char]
printBoardHelper board bSize column print
 | board == "" = "\n" ++ print
 | column == bSize = print ++ [(head board)] ++ " " ++ "\n" ++ printBoardHelper (tail board) bSize 1 print
 | otherwise = print ++ [(head board)] ++ " " ++ (printBoardHelper (tail board) bSize (column+1) print)

-- | Puts 'new' into element #i
replaceNth :: Int -> Char -> [Char] -> [Char]
replaceNth i new (x:xs)
 | i == 0 = new:xs
 | otherwise = x:replaceNth (i-1) new xs
