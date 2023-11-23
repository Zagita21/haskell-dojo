module HM.A7 where

import Data.Char (isAlpha, toLower, toUpper)
import HM.A6
import HM.Provided
import System.Directory (doesFileExist)
import Data.List (intersperse, sort)

-- Q#01
data Game = Game { getSecret :: Secret, getCurrentGuess :: Guess, getMoves :: [Char], getChances :: Chances }

-- Q#02
repeatedMove :: Move -> Game -> Bool
repeatedMove m g = elem m (getMoves g) 
-- repeatedMove m g = (m `elem` getMoves g) 

-- Q#03
makeGame :: Secret -> Game
makeGame s = Game { 
    getSecret = map toUpper s,
    getCurrentGuess = startGuess,
    getMoves =[],
    getChances = _CHANCES_
  } 
  where
    startGuess = map (const '-') s -- take (length s) $ repeat '_'

-- Q#04
updateGame :: Move -> Game -> Game
updateGame m g = g {
    getCurrentGuess = revealLetters (toUpper m) secret previousGuess,
    getMoves = updatedMoves,
    getChances = chancesLeft  
  }
  where
    secret = getSecret g
    previousGuess = getCurrentGuess g
    chancesLeft = updateChances m secret (getChances g)
    updatedMoves = if repeatedMove m g then getMoves g else m:(getMoves g)
-- Q#05
instance Show Game where
  show g = 
    unlines
      [
        _STARS_,
        "\tYour guess:\t" ++ intersperse ' ' guess ++ "\n",
        "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
        "\tChances:\t" ++ show chances,
        _STARS_
      ]
    where
      (moves, chances, guess) = (getMoves g, getChances g, getCurrentGuess g)

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper secret moves chances =
  unlines
    [ _STARS_,
      "\tSecret Word:\t" ++ intersperse ' ' secret ++ "\n",
      "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n",
      "\tChances:\t" ++ show chances,
      _STARS_
    ]

--test
testg = makeGame "babboon"

-- Q#06
{- data GameException = InvalidChars
 | InvalidLength
 | NotInDict
 | InvalidMove
 | RepeatMove
 | GameOver -}
instance Show GameException where
  show InvalidChars = "Invalid characters, expecting alphabet chars"
  show InvalidLength = "Length invalid, needs to be between " ++ lb ++ " and " ++ ub ++ " characters long"
    where
      lb = show $ fst _LENGTH_
      ub = show $ snd _LENGTH_
  show NotInDict = "Not in dictionary!"
  show InvalidMove = "Invalid move"
  show RepeatMove = "Repeat move (you've already guessed that!)"
  show GameOver = "Game over!"
    

-- Q#07
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True x = Just x

-- Q#08
validateSecret :: ( Secret -> Bool ) -> GameException -> Secret -> Either GameException Secret
validateSecret predicate gameException s =
  if predicate s then Right s else Left gameException

-- Q#09
hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret allAlpha InvalidChars
  where
    allAlpha :: Secret -> Bool
    allAlpha = and . map isAlpha

isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict d = validateSecret (\s -> (map toLower s) `elem` d) NotInDict

-- Q#10
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
  Right s -> isValidLength s 
  Left ge -> Left ge

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case validateNoDict s of
  Right s -> isInDict d s 
  Left ge -> Left ge

-- Q#11
processTurn :: Move -> Game -> Either GameException Game
processTurn m g
  | invalidMove m = Left InvalidMove
  | repeatedMove m g = Left RepeatMove
  | chancesOut = Left GameOver
  | otherwise = Right updatedGame
  where
    updatedGame = updateGame m g
    chancesOut = getChances updatedGame == 0