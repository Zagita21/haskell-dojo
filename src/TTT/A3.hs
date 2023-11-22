module TTT.A3 where

import Data.List (transpose)
import TTT.A1
import TTT.A2

-- Q#01
showInts :: [Int] -> [String]
showInts [] = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: String
_HEADER_ = " " ++ formatLine ( showInts _RANGE_ )

-- Q#02
showSquares :: [Square] -> [String]
showSquares [] = []
showSquares (x:xs) = showSquare x : showSquares xs

-- Q#03
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (x:xs) = formatLine (showSquares x) : formatRows xs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty [] _ = False
isColEmpty (x:_) 0 = x == Empty
isColEmpty (_:xs) n 
  | n < _SIZE_ = isColEmpty xs (n-1)
  | otherwise = False

isColEmpty' :: Row -> Int -> Bool
isColEmpty' [] _ = False
isColEmpty' r c
  | c >= 0 && c < _SIZE_ = r !! c == Empty 
  | otherwise = False

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol [] = []
dropFirstCol (r:rs) = tail r : dropFirstCol rs

dropLastCol :: Board -> Board
dropLastCol [] = []
dropLastCol (r:rs) = top r : dropLastCol rs 
  where
    top = take (len - 1)
    len = length r

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (r:rs) = r !! 0 : getDiag1 reduced
  where
    reduced = dropFirstCol rs

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (r:rs) = last r : getDiag2 reduced
  where
    reduced = dropLastCol rs

getAllLines:: Board -> [Line]
getAllLines b =  concat [ b , transpose b, diags ]
  where
    diags = [getDiag1 b, getDiag2 b]

-- Q#07
putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare p (r:rs) (0,y) = replaceSquareInRow p y r : rs
putSquare p (r:rs) (x,y) = r : putSquare p rs (x-1, y)

-- Q#08
prependRowIndices :: [String] -> [String]
prependRowIndices ss = worker ( zip ['A'..] ss )
  where
    worker :: [(Char, String)] -> [String]
    worker [] = []
    worker ((c, s):css) = (c:s): worker css 

-- Q#09
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ _ [] = False
isWinningLine_ p line = worker False line
  where
    worker acc [] = acc
    worker acc (l:ls) 
      | acc == False  && l == p = worker True ls
      | l /= p = False
      | otherwise = worker acc ls
    

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove rs (x,y)
  | isMoveInBounds (x, y) = worker rs (x, y)
  | otherwise = False
    where
      worker [] _ = False
      worker (r:_) (0, y) = isColEmpty r y
      worker (_:rs) (x, y) = worker rs (x-1, y)

