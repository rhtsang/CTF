import Data.List
import Data.Char

-- d = 1 should capture, d = 2 should realize that capturing will lose the game
test0w n = capture ["-------------------------"] 'w' n
test0b n = capture ["-------------------------"] 'b' n
test1 n = capture ["bWww-b-B-"] 'w' n
test2 n = capture ["-W-w-bbBw"] 'b' n
test3 n = capture ["www---bBb"] 'w' n
test3b n = capture ["www---bBb"] 'b' n
test4w n = capture ["wWBb"] 'w' n
test5w n = capture ["bW----wB-"] 'w' n
test5b n = capture ["Wb----wB-"] 'b' n
test6w n = capture [""] 'w' n

-- top-level capture function that tells the user what move to make
-- inputs:
--   (b:h): list of current board and past boards
--   p: the player's turn, 'b' or 'w'
--   d: the depth to pass into the minimax function
capture :: [[Char]] -> Char -> Int -> [Char]
capture (b:h) _ 0 = b
capture (b:history) p d
 | moves (b:history) p == [] = []
 | p == 'w' = (moves (b:history) p)!!(maxi $ map (minimax (other p) (d-1) history) (moves (b:history) p))
 | p == 'b' = (moves (b:history) p)!!(mini $ map (minimax (other p) (d-1) history) (moves (b:history) p))

-- | White gets possitive points for having advantage and black gets negative
-- | points. So black always tries to minimize the score to win and white
-- | always tries to maximize the score win.
-- | intput: player depth history board
-- | returns the minimax eval of the given board for other params
minimax :: Char -> Int -> [[Char]] -> [Char] -> Int
minimax p 0 h b = staticeval p h b
minimax p d h b
-- | empty list check
 | (moves (b:(h++[b])) p) == [] = 0
 -- | win p (b:h) == 1 = staticeval p h b
-- | max level. White tries to get maximum score always
 | p == 'w' = mymax $ map (minimax (other p) (d-1) (h++[b])) (moves (b:(h++[b])) p)
-- | min level. Black tries to get minimal score always
 | p == 'b' = mymin $ map (minimax (other p) (d-1) (h++[b])) (moves (b:(h++[b])) p)


-- strategy: Each piece has a value, 1 for pawn and 500000 for flag.
--   The more possible moves a board has, the more points it has.
--   The closer a pawn is to the enemy flag, the better the board is.
--   Similarly, the closer your flag is to the enemy side, the better the board is. 
-- | input: player history board
-- | output: score of the board for this player
staticeval :: Char -> [[Char]] -> [Char] -> Int
staticeval p h b
 | p == 'w' = (evalPieces b) + (evalMoves (b:h) p) + (pawnToFlag (elemIndices p b) (elemIndices (flag p) b) (boardSize b) 99999) + (flagToEnemy 'W' b)
 | p == 'b' = (evalPieces b) + (evalMoves (b:h) p) - (pawnToFlag (elemIndices p b) (elemIndices (flag p) b) (boardSize b) 99999) - (flagToEnemy 'B' b)

-- | Static eval of board by counting pieces. Kings 100000, pawns 1.
-- | Input: single board string, Output: Points+
evalPieces :: [Char] -> Int
evalPieces [] = 0
evalPieces (x:xs)
 | x == 'w'  = evalPieces xs + 1
 | x == 'b'  = evalPieces xs - 1
 | x == 'W'  = evalPieces xs + 500000
 | x == 'B'  = evalPieces xs - 500000
 | otherwise = evalPieces xs

-- | Static eval of board by counting possible moves
-- | if its white turn and there are no white moves white loses. same for black
evalMoves :: [[Char]] -> Char -> Int
evalMoves [] _ = 0
evalMoves (b:history) p
 | (length (genMoves b history (getIndices p b) [])) == 0 = -100000
 | (length (genMoves b history (getIndices (other p) b) [])) == 0 = 100000
 | otherwise = 0

-- smallest distance between pawn and enemy flag; lower distance => higher board evaluation value
-- uses Manhattan distance to calculate this value
-- first call should use minDistance = 9999 or some other really high value
-- end result is negated so that a higher distance will result in a lower evaluation
-- inputs: indices of pawns, flag indices, board size, minimum distance (to begin comparing with)
-- output: value of board based on how close your pawn is to capturing enemy flag
pawnToFlag :: [Int] -> [Int] -> Int -> Int -> Int
pawnToFlag [] flagInds boardSize minDistance = -1*minDistance
pawnToFlag (i:indices) flagInds boardSize minDistance
 | flagInds == [] = 99999
 | flagInds == [] = minDistance
 | otherwise = pawnToFlag indices flagInds boardSize (min minDistance ((abs((div i boardSize)-(div (head flagInds) boardSize))) + abs((mod i boardSize)-(mod (head flagInds) boardSize))))

-- distance from flag to enemy side; simple y-axis calculation
-- similar to pawnToFlag above
-- inputs: flag 'B' or 'W', board
-- output: value of board based on how close flag is to enemy side
flagToEnemy ::  Char -> [Char] -> Int
flagToEnemy flag board
 | (elemIndices flag board) == [] = -99999
 | flag == 'B' = -1*(div (head (elemIndices flag board)) (boardSize board))
 | otherwise = -1*((boardSize board) - (head (elemIndices flag board)))

-- | Helper functions :

-- | gets index of max element of list
maxi :: (Ord a, Ord c, Num c, Enum c) => [a] -> c
maxi xs = snd . head . reverse . sort $ zip xs [0..]

-- | gets index of min element of list
mini :: (Ord a, Ord c, Num c, Enum c) => [a] -> c
mini xs = snd . head . sort $ zip xs [0..]

-- wrapper function for maximum to handle empty lists
mymax :: [Int] -> Int
mymax [] = -99999
mymax boards = maximum boards

-- wrapper function for minimum to handle empty lists
mymin :: [Int] -> Int
mymin [] = 99999
mymin boards = minimum boards
-- | Switching player in functions
other :: Char -> Char
other 'w' = 'b'
other 'b' = 'w'

-- | Switching player in functions
flag  :: Char -> Char
flag 'w' = 'W'
flag 'b' = 'B'

-- | Switching player in functions
otherflag  :: Char -> Char
otherflag 'w' = 'B'
otherflag 'b' = 'W'

-- | input: player, board
-- | output: list of indices of the players pieces
getIndices :: Char -> [Char] -> [Int]
getIndices 'w' board = (elemIndices 'w' board) ++ (elemIndices 'W' board)
getIndices 'b' board = (elemIndices 'b' board) ++ (elemIndices 'B' board)

-- | Calculates the dimension of the board
boardSize :: [Char] -> Int
boardSize board = (round (sqrt (fromIntegral (length board))))

-- A player wins if
-- 1) all the opponent's pawns have been captured
-- 2) the opponent's flag has been captured
-- 3) it's the opponent's turn to move and the opponent can't make a legal move
-- 4) the player moves his flag forward past all the opponent's pawns without the flag being captured.
-- input: player and board
-- output: 1 - wins, 0 - not wins
win :: Char -> [[Char]] -> Int
win p (b:h)
 | elemIndices (flag p) b == []       = 0
 | elemIndices p b == []              = 0
 | elemIndices (other p) b == []      = 1
 | elemIndices (otherflag p) b == []  = 1
 | moves (b:h) (other p) == []        = 1
 | p == 'w' && (((maximum $ elemIndices 'W' b) - (maximum $ elemIndices 'b' b)) > (boardSize b)) = 1
 | p == 'b' && (((minimum $ elemIndices 'B' b) - (minimum $ elemIndices 'w' b)) < (boardSize b)) = 1
 | otherwise = 0


-- | Generating moves:

-- | wrappper funciton
moves :: [[Char]] -> Char -> [[Char]]
moves (b:h) p
 | elemIndices 'W' b == [] = []
 | elemIndices 'B' b == [] = []
 | elemIndices 'w' b == [] = []
 | elemIndices 'b' b == [] = []
 | otherwise = genMoves b h (getIndices p b) []

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

-- | Printing boards:

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
