module HM.A6 where

import Data.Char (isAlpha, toUpper)
import HM.Provided

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException = InvalidChars
 | InvalidLength
 | NotInDict
 | InvalidMove
 | RepeatMove
 | GameOver

-- Q#03
lengthInRange :: Secret -> Bool
lengthInRange s = let (shortest, longest) = _LENGTH_ in
  length s >= shortest && length s <= longest 

-- Q#04
invalidMove :: Char -> Bool 
invalidMove = not . isAlpha

-- Q#05
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m = zipWith (\x y -> if x == m then x else y)

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c = let goodGuess = (toUpper m) `elem` s in
  if goodGuess then c else c - 1

-- Q#07
setSecret :: IO String
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  input <- getLine
  showInput True
  _SPACE_
  return input
