import Data.List
import Data.Char

{-
5x5 board numberation
 0  1  2  3  4
 5  6  7  8  9
10 11 12 13 14
15 16 17 18 19
20 21 22 23 24
-}

-- | Default 5x5 board
test1 = printAllBoards (genMoves "-wWw--www-------bbb--bBb-" [] [6,7] [])
test2 = printAllBoards (genMoves "-wWw-w-ww-------bbb--bBb-" ["-wWw--www-------bbb--bBb-"] [6,7] [])
test3 = printAllBoards (genMoves "-Ww---bB-" [] [2] [])

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

boardSize board = (round (sqrt (fromIntegral (length board))))

genMoves :: [Char] -> [[Char]] -> [Int]-> [[Char]] -> [[Char]]
genMoves board history []      moves = filter (\n -> not (elem n history)) (filter (not . null) moves)
genMoves board history indices moves = genMoves board history (tail indices) (moves++(genMovesHelper board indices))

genMovesHelper :: [Char] -> [Int] -> [[Char]]
genMovesHelper board [] = []
genMovesHelper board indices =
    [move1Up board (head indices) (boardSize board)]++
    [move1Down board (head indices) (boardSize board)]++
    [move1Left board (head indices) (boardSize board)]++
    [move1Right board (head indices) (boardSize board)]++
    [move2Up board (head indices) (boardSize board)]++
    [move2Down board (head indices) (boardSize board)]++
    [move2Left board (head indices) (boardSize board)]++
    [move2Right board (head indices) (boardSize board)]


move1Up :: [Char] -> Int->  Int -> [Char]
move1Up currBoard index boardSize
 | index-boardSize >= 0 &&
   currBoard!!(index-boardSize) == '-'
    = replaceNth index '-' (replaceNth (index-boardSize) (currBoard!!index) currBoard)
 | otherwise = []

move1Down :: [Char] -> Int->  Int -> [Char]
move1Down currBoard  index boardSize
 | index+boardSize < boardSize^2 &&
   currBoard!!(index+boardSize) == '-'
    = replaceNth index '-' (replaceNth (index+boardSize) (currBoard!!index) currBoard)
 | otherwise = []

move1Left :: [Char] -> Int->  Int -> [Char]
move1Left currBoard  index boardSize
 | index-1 >= 0 &&
   index `mod` boardSize /= 0 &&
   currBoard!!(index-1) == '-'
    = replaceNth index '-' (replaceNth (index-1) (currBoard!!index) currBoard)
 | otherwise = []

move1Right :: [Char] -> Int->  Int -> [Char]
move1Right currBoard  index boardSize
 | index+1 < boardSize^2 &&
   (index+1) `mod` boardSize /= 0 &&
   currBoard!!(index+1) == '-'
    = replaceNth index '-' (replaceNth (index+1) (currBoard!!index) currBoard)
 | otherwise = []

move2Up :: [Char] -> Int->  Int -> [Char]
move2Up currBoard  index boardSize
 | (currBoard!!index) == 'b' &&
   index-(2*boardSize) >= 0 &&
   currBoard!!(index-(2*boardSize)) == '-' &&
   (currBoard!!(index-boardSize) == 'w' ||
   currBoard!!(index-boardSize) == 'W')
    = replaceNth (index-(2*boardSize)) (currBoard!!index) (replaceNth index '-' (replaceNth (index-boardSize) '-' currBoard))
 | otherwise = []

move2Down :: [Char] -> Int->  Int -> [Char]
move2Down currBoard  index boardSize
 | (currBoard!!index) == 'w' &&
   index+(2*boardSize) < boardSize^2 &&
   currBoard!!(index+(2*boardSize)) == '-' &&
   (currBoard!!(index+boardSize) == 'b' ||
   currBoard!!(index+boardSize) == 'B')
   = replaceNth (index+(2*boardSize)) (currBoard!!index) (replaceNth index '-' (replaceNth (index+boardSize) '-' currBoard))
 | otherwise = []

move2Left :: [Char] -> Int->  Int -> [Char]
move2Left currBoard  index boardSize
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
move2Right currBoard index boardSize
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
-- printBoard :: [Char] -> Int -> [Char] -> IO()
-- printBoard board boardSize print = putStr (printBoardHelper board boardSize 1 [])

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
