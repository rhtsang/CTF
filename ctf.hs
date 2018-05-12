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
-}

-- | Default 5x5 board
test1 = printAllBoards (genMoves "-wWw--www-------bbb--bBb-" [] [6,7] [])
test2 = printAllBoards (genMoves "-wWw-w-ww-------bbb--bBb-" ["-wWw--www-------bbb--bBb-"] [6,7] [])
test3 = printAllBoards (genMoves "-Ww---bB-" [] [1, 2] [])
test4 = printAllBoards (genMoves "-W---b-Bw" [] [1, 8] [])
test5 = map evalWhitePieces (genMoves "-W---b-Bw" [] [1, 8] [])

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


move1Up :: [Char] -> Int->  Int -> [Char]
move1Up currBoard boardSize index
 | index-boardSize >= 0 &&
   currBoard!!(index-boardSize) == '-'
    = replaceNth index '-' (replaceNth (index-boardSize) (currBoard!!index) currBoard)
 | otherwise = []

move1Down :: [Char] -> Int->  Int -> [Char]
move1Down currBoard  boardSize index
 | index+boardSize < boardSize^2 &&
   currBoard!!(index+boardSize) == '-'
    = replaceNth index '-' (replaceNth (index+boardSize) (currBoard!!index) currBoard)
 | otherwise = []

move1Left :: [Char] -> Int->  Int -> [Char]
move1Left currBoard  boardSize index
 | index-1 >= 0 &&
   index `mod` boardSize /= 0 &&
   currBoard!!(index-1) == '-'
    = replaceNth index '-' (replaceNth (index-1) (currBoard!!index) currBoard)
 | otherwise = []

move1Right :: [Char] -> Int->  Int -> [Char]
move1Right currBoard  boardSize index
 | index+1 < boardSize^2 &&
   (index+1) `mod` boardSize /= 0 &&
   currBoard!!(index+1) == '-'
    = replaceNth index '-' (replaceNth (index+1) (currBoard!!index) currBoard)
 | otherwise = []

-- | Only black pawns can move 2 up and they have to jump over w or W
move2Up :: [Char] -> Int->  Int -> [Char]
move2Up currBoard  boardSize index
 | (currBoard!!index) == 'b' &&
   index-(2*boardSize) >= 0 &&
   currBoard!!(index-(2*boardSize)) == '-' &&
   (currBoard!!(index-boardSize) == 'w' ||
   currBoard!!(index-boardSize) == 'W')
    = replaceNth (index-(2*boardSize)) (currBoard!!index) (replaceNth index '-' (replaceNth (index-boardSize) '-' currBoard))
 | otherwise = []

-- | Only white pawns can move 2 down and they have to jump over b or B
move2Down :: [Char] -> Int->  Int -> [Char]
move2Down currBoard  boardSize index
 | (currBoard!!index) == 'w' &&
   index+(2*boardSize) < boardSize^2 &&
   currBoard!!(index+(2*boardSize)) == '-' &&
   (currBoard!!(index+boardSize) == 'b' ||
   currBoard!!(index+boardSize) == 'B')
   = replaceNth (index+(2*boardSize)) (currBoard!!index) (replaceNth index '-' (replaceNth (index+boardSize) '-' currBoard))
 | otherwise = []

move2Left :: [Char] -> Int->  Int -> [Char]
move2Left currBoard  boardSize index
 | (currBoard!!index) == 'b' &&
   index-2 >= 0 &&
   (index-1) `mod` boardSize /= 0 &&
   index `mod` boardSize /= 0 &&
   currBoard!!(index-2) == '-' &&
   (currBoard!!(index-1) == 'w' ||
   currBoard!!(index-1) == 'W')
    = replaceNth (index-2) (currBoard!!index) (replaceNth index '-' (replaceNth (index-1) '-' currBoard))
 | (currBoard!!index) == 'w' &&
   index-2 >= 0 &&
   (index-1) `mod` boardSize /= 0 &&
   index `mod` boardSize /= 0 &&
   currBoard!!(index-2) == '-' &&
   (currBoard!!(index-1) == 'b' ||
   currBoard!!(index-1) == 'B')
     = replaceNth (index-2) (currBoard!!index) (replaceNth index '-' (replaceNth (index-1) '-' currBoard))
 | otherwise = []

move2Right :: [Char] -> Int->  Int -> [Char]
move2Right currBoard boardSize index
 | (currBoard!!index) == 'b' &&
   index+2 < boardSize^2 &&
   (index+1) `mod` boardSize /= 0 &&
   (index+2) `mod` boardSize /= 0 &&
   currBoard!!(index+2) == '-' &&
   (currBoard!!(index+1) == 'w' ||
   currBoard!!(index+1) == 'W')
    = replaceNth (index+2) (currBoard!!index) (replaceNth index '-' (replaceNth (index+1) '-' currBoard))
 | (currBoard!!index) == 'w' &&
   index+2 < boardSize^2 &&
   (index+1) `mod` boardSize /= 0 &&
   (index+2) `mod` boardSize /= 0 &&
   currBoard!!(index+2) == '-' &&
   (currBoard!!(index+1) == 'b' ||
   currBoard!!(index+1) == 'B')
    = replaceNth (index+2) (currBoard!!index) (replaceNth index '-' (replaceNth (index+1) '-' currBoard))
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
-- | printBoard :: [Char] -> IO()
-- | printBoard board = putStr (printBoardHelper board (boardSize board) 1 [])

-- | Tail Recursive helper. Stores the output in 'print'
-- | Breaks down a board string using board size.
printBoardHelper :: [Char] -> Int -> Int -> [Char] -> [Char]
printBoardHelper board bSize column print
 | board == "" = "\n" ++ print
 | column == bSize = print ++ [(head board)] ++ " " ++ "\n" ++ printBoardHelper (tail board) bSize 1 print
 | otherwise = print ++ [(head board)] ++ " " ++ (printBoardHelper (tail board) bSize (column+1) print)

-- | Puts 'new' into element #index
replaceNth :: Int -> Char -> [Char] -> [Char]
replaceNth index new (x:xs)
 | index == 0 = new:xs
 | otherwise = x:replaceNth (index-1) new xs
