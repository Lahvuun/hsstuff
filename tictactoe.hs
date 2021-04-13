import Data.List
import System.IO

type Row = [Char]

type Board = [Row]

type Team = Char

type Teams = [Team]

type Cell = Int

type Move = (Int, Int)

data Result
  = Victory
  | Draw
  | Unfinished

makeBoard :: Int -> Board
makeBoard size = replicate size (replicate size ' ')

checkValid :: Move -> Board -> Bool
checkValid (x, y) board
  | y < 0 = False
  | x >= size = False
  | y >= size = False
  | board !! y !! x /= ' ' = False
  | otherwise = True
  where
    size = length board

makeMove :: Cell -> Int -> Move
makeMove cell size = (cell' `mod` size, size - 1 - cell' `div` size)
  where
    cell' = cell - 1

updateBoard :: Board -> Move -> Team -> Board
updateBoard board (x, y) team =
  [ [ if x == x' && y == y'
    then team
    else board !! y' !! x'
  | x' <- [0 .. size]
  ]
  | y' <- [0 .. size]
  ]
  where
    size = length board - 1

checkRowWin :: Row -> Bool
checkRowWin (c:cs)
  | c == ' ' = False
  | length (nub (c : cs)) > 1 = False
  | otherwise = True

checkDiagonalWin :: Board -> Bool
checkDiagonalWin board =
  checkRowWin [board !! x !! x | x <- [0 .. size]] ||
  checkRowWin [board !! (size - x) !! x | x <- [0 .. size]]
  where
    size = length board - 1

checkBoard :: Board -> Result
checkBoard board
  | any checkRowWin board = Victory
  | any checkRowWin (transpose board) = Victory
  | checkDiagonalWin board = Victory
  | all (/= ' ') (concat board) = Draw
  | otherwise = Unfinished

getMove :: Board -> IO Move
getMove board = do
  putStr "Make a move: "
  hFlush stdout
  line <- getLine
  let move = makeMove (read line) (length board)
  if checkValid move board
    then return move
    else do
      putStrLn "Illegal move"
      getMove board

putBoardLn :: Board -> IO ()
putBoardLn [] = return ()
putBoardLn (r:rs) = do
  putStrLn (show r)
  putBoardLn rs

play :: Board -> Teams -> IO ()
play board (team:teams) = do
  putStr "Player: "
  putChar team
  putStrLn ""
  move <- getMove board
  let newBoard = updateBoard board move team
  putBoardLn newBoard
  case checkBoard newBoard of
    Victory -> do
      putStr "Player: "
      putChar team
      putStrLn " won!"
    Draw -> putStrLn "Draw"
    Unfinished -> play newBoard teams

main :: IO ()
main = play (makeBoard 3) (cycle ['X', 'O'])
