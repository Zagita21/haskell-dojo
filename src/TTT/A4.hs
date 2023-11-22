module TTT.A4 where

import Data.List (transpose)
import TTT.A1
import TTT.A2
import TTT.A3 (getAllLines, putSquare)

-- Q#01
_HEADER_ :: String
-- _HEADER_ = " " ++ formatLine ( showInts _RANGE_ )
_HEADER_ = " " ++ formatLine (map show _RANGE_ )

-- Q#02
showSquares :: [Square] -> [String]
{- showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs -}
showSquares = map showSquare

-- Q#03
dropFirstCol :: Board -> Board
{- dropFirstCol [] = []
dropFirstCol (r:rs) = tail r : dropFirstCol rs -}
dropFirstCol = map tail

-- Q#04
dropLastCol :: Board -> Board
{- dropLastCol [] = []
dropLastCol (r:rs) = top r : dropLastCol rs 
  where
    top = take (len - 1)
    len = length r -}
dropLastCol = map top
  where
    top =  reverse . tail . reverse

--Q#05
formatRows :: [Row] -> [String]
{- formatRows [] = []
formatRows (x:xs) = formatLine (showSquares x) : formatRows xs -}
formatRows = map (\r -> formatLine . showSquares $ r)

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
{- isWinningLine_ _ [] = False
isWinningLine_ p line = worker False line
  where
    worker acc [] = acc
    worker acc (l:ls) 
      | acc == False  && l == p = worker True ls
      | l /= p = False
      | otherwise = worker acc ls -}
isWinningLine_ _ [] = False
isWinningLine_ p line = length others == 0
  where
    others = filter (/=p) line

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine _ [] = False
isWinningLine p line = foldr reducer True line
  where
    reducer val acc = acc && val == p

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon _ [] = False
hasWon p b = foldr reducer False lines
  where
    lines = getAllLines b 
    reducer line acc = acc || isWinningLine p line

--useful testing
_X_WIN_ = [ [X, O, O], [O, X, O], [O,O,X]]
_O_WIN_ = [ [O, X, O], [X, X, O], [X, O, O]]

-- Q#09
getGameState :: Board -> GameState
getGameState b
  | hasWon X b = XWins
  | hasWon O b = OWins
  | isTied b = Draw
  | otherwise = InProgress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = (gameState, newboard)
  where
    newboard = putSquare p b m
    gameState = getGameState newboard

-- Q#10
prependRowIndices :: [String] -> [String]
{- prependRowIndices ss = worker ( zip ['A'..] ss )
  where
    worker :: [(Char, String)] -> [String]
    worker [] = []
    worker ((c, s):css) = (c:s): worker css  -}
prependRowIndices = zipWith (:) ['A'..]

-- Q#11
formatBoard :: Board -> String
formatBoard b = unlines . (_HEADER_ :) . prependRowIndices . formatRows $ b