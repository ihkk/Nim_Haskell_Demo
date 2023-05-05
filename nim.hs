-- a programme to implement nim game (people vs people)
-- Author: Kai HE & GitHub Copilot

import System.IO

-- main function
type Board = [Int]

-- a function to get the number of stars in a line of a board
getLineStars :: Board -> Int -> Int
getLineStars b n = b !! n

-- a function to convert a Line with stars to string
printLine :: Board -> Int -> String
printLine b n = show (n + 1) ++ ": " ++ replicate (getLineStars b n) '*' ++ "\n"

-- a function to print a Board with five lines, and each line starting with a number (1-5)
-- for example, "1: *****\n2: ****\n3: ***\n4: **\n5: *\n"
showBoard :: Board -> String
showBoard b = concat [printLine b n | n <- [0 .. 4]]

-- a function to print out a board
printBoard :: Board -> IO ()
printBoard b = putStr (showBoard b)

-- a function to set a line of a board
setLine :: Board -> Int -> Int -> Board
setLine b n v = take n b ++ [v] ++ drop (n + 1) b

-- a function to initialize a board with 5-1
initBoard :: Board
initBoard = [5, 4, 3, 2, 1]

-- a function to create a blank board
blankBoard :: Board
blankBoard = [0, 0, 0, 0, 0]

-- a function to set five lines of a board at one time, by inputing a list of numbers
setBoard :: Board -> [Int] -> Board
setBoard b [] = b
setBoard b (x : xs) = setBoard (setLine b (5 - length xs) x) xs

-- a function to remove y stars from line x of board b
removeStars :: (Int, Int) -> Board -> Board
removeStars (x, y) b = setLine b x (getLineStars b x - y)

-- check if the game is over
isOver :: Board -> Bool
isOver b = sum b == 0

-- read user instructions as line and number of stars
userInput :: IO (Int, Int)
userInput = do
  putStr "Line: "
  l <- getLine
  putStr "Stars: "
  s <- getLine
  return (read l, read s)

-- repeat user input until legal move
userInputRepeat :: Board -> IO Board
userInputRepeat b = do
  (l, s) <- userInput
  if l > 5 || l < 1 || s > getLineStars b (l - 1) || s < 1
    then do
      putStrLn "Illegal move"
      userInputRepeat b
    else return (removeStars (l - 1, s) b)

-- print which player's turn
printPlayer :: Int -> IO ()
printPlayer n = putStrLn ("Player " ++ show n ++ "'s turn")

-- switch player
switchPlayer :: Int -> Int
switchPlayer 1 = 2
switchPlayer 2 = 1

-- a function to play nim game on a board by switching player 1 and 2
playNimBoard :: Board -> Int -> IO ()
playNimBoard b player = do
  putStrLn "====================="
  printPlayer player
  printBoard b
  b' <- userInputRepeat b
  if isOver b'
    then do
      putStrLn "====================="
      putStrLn ("Player " ++ show player ++ " wins!\n")
      return ()
    else playNimBoard b' (switchPlayer player)

-- a function to play nim game
playNim :: IO ()
playNim = do
  putStrLn "Welcome to nim game!"
  playNimBoard initBoard 1
