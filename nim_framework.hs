-- provided by Tony

type Board = [Int]

showBoard :: Board -> String
-- removeStars (x,y) b : remove y stars from line x of board b.
removeStars :: (Int, Int) -> Board -> Board
-- read user instructions as line and number of stars
userInput :: IO (Int, Int)
-- repeat user input until legal move
userInputRepeat :: Board -> IO Board
playNim :: IO ()
