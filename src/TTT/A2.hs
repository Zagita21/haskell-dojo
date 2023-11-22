module TTT.A2 where

import Data.List (intercalate)
import TTT.A1

-- Q#01
promptPlayer :: Player -> String
promptPlayer p = concat ["Player ", showSquare p, "'s turn:  enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0.. _SIZE_-1]

-- Q#03
isDigit :: Char -> Bool
isDigit c =
  c `elem` digits
  where
    digits = ['0'..'9'] 

readDigit :: Char -> Int
readDigit c = case isDigit c of
                False -> -1
                True -> read ( c :[] )  

-- Q#04
_EMPTY_ROW_ :: [Square]
_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ :: [Row]
_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05
isTied :: Board -> Bool
isTied b = Empty `notElem` (concat b)

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
    [X, O, O]
  , [O, X, X]
  , [O, X, O]
  ]

-- Q#06
indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings ss = zip ['A'..] ss 

-- Q#07
formatLine :: [String] -> String
formatLine ss = concat [ _SEP_ , interc, _SEP_]
  where
    interc = intercalate _SEP_ ss

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (x, y) = 
  let
    inside = [x >= 0 && x <  _SIZE_, y >= 0 && y <  _SIZE_ ] 
  in
    and inside 

-- Q#09
stringToMove :: String -> Move
stringToMove (c1:c2:[]) 
  | isMoveInBounds (row, col)  = (row, col)
  | otherwise = _INVALID_MOVE_
  where 
      row = convertRowIndex c1
      col = readDigit c2
{- stringToMove (c1:c2:[]) = if outOfRange col || outOfRange row then _INVALID_MOVE_ else (row, col)
  where 
    outOfRange d = d < 0 || d >= _SIZE_
    row = convertRowIndex c1
    col = readDigit c2
stringToMove _ = _INVALID_MOVE_ -}
  

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow _ _ [] = []  
replaceSquareInRow p i r = concat [firstPart, adjustedPart]
  where
    (firstPart, secondPart) = splitAt i r
    adjustedPart = if i >= _SIZE_ || i < 0 then secondPart
                    else
                      p : tailOrEmptyList
    tailOrEmptyList = if secondPart == [] then [] else tail secondPart

rsX :: Int -> Row -> Row
rsX = replaceSquareInRow X

rsO :: Int -> Row -> Row
rsO = replaceSquareInRow O
