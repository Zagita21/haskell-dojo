module TTT.A1 where

import Data.Char (toUpper)

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
convertRowIndex :: Char -> Int
convertRowIndex = (\x -> x - 65) . fromEnum . toUpper

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = "_|_"

-- Q#06

data Square = X | O | Empty deriving (Show, Eq, Read)

-- Q#07

data GameState = XWins | OWins | Draw | InProgress deriving (Show, Eq, Read)

-- Q#08
type Player = Square
type Row = [Square]
type Line = [Square]
type Board = [Row]
type Move = (Int, Int)

-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer x = if x then X else O

getFirstPlayer_ :: Bool -> Player
getFirstPlayer_ x
  | x == True = X
  | otherwise = O

-- Q#10

showGameState :: GameState -> String
showGameState g = case g of
                    XWins -> "X is the winner"
                    OWins -> "Y is the winner"
                    Draw -> "It's a draw"
                    InProgress -> "Game in progress"

showGameState' :: GameState -> String
showGameState' = show


-- Q#11
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer _ = Empty


-- Q#12
showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare _ = "_"

showSquare' :: Square -> String
showSquare' x = if x == Empty then "_" else show x  