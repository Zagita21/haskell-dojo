module HM.A8 where

import Control.Monad (when)
import Data.Char (toUpper)
import HM.A6
import HM.A7 hiding (validateNoDict, validateWithDict)
import HM.Provided
import System.Directory (doesFileExist)

-- Q#01
getUpperChar :: IO Char
-- getUpperChar = fmap toUpper getChar
getUpperChar = toUpper <$> getChar

-- Q#02
_DICT_ :: IO Dictionary
_DICT_ = do
  fileExists <- doesFileExist _DICT_FILE_
  if fileExists then pure $ words _DICT_FILE_ else pure []

isDictNonEmpty :: IO Bool
-- isDictNonEmpty = fmap ((> 0) . length) $ _DICT_
isDictNonEmpty = ((> 0) . length) <$> _DICT_

-- Q#03
makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid = fmap makeGame

-- Q#04
-- getDict :: IO (Dictionary -> Maybe Dictionary) -> IO Dictionary -> IO (Maybe Dictionary)
getDict :: IO (Maybe Dictionary)
getDict = ioToMaybe <*> _DICT_
  where 
    ioToMaybe :: IO (a -> Maybe a)
    ioToMaybe = toMaybe <$> isDictNonEmpty

-- Q#05
{- validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
  Right s -> isValidLength s 
  Left ge -> Left ge

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case validateNoDict s of
  Right s -> isInDict d s 
  Left ge -> Left ge -}
validateNoDict :: Secret -> Either GameException Secret
validateNoDict x = hasValidChars x >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d x = validateNoDict x >>= isInDict d

-- Q#06
playGame :: Game -> IO ()
playGame g = do
  promptGuess
  m <- getUpperChar
  _SPACE_
  let turn = processTurn m g
  case turn of 
    Left GameOver -> 
      (putStrLn $ show GameOver) >> 
      putStrLn ("The secret word was " ++ getSecret g) >>
      return ()
    Left exception -> 
      putStrLn ("Invalid move" ++ show exception) >>
      playGame g
    Right newg -> putStrLn (show newg) >> --return ()
      if getCurrentGuess newg == getSecret newg then
        putStrLn "You got it! Well done!"
      else
        playGame newg

-- Q#07
startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame validator = do
  s <- setSecret
  let eGame = makeGameIfValid $ validator s
  case eGame of
    Left exception ->
      do
        putStrLn (show exception)
        startGame validator
    Right g ->
      do
        putStrLn $ show g 
        playGame g


-- Q#08

runHM :: IO ()
runHM = do
  maybeDict <- getDict
  case maybeDict of
    {- Just d ->
      startGame $ validateWithDict d
    Nothing  -}
    _ ->
      do
        putStrLn "Missing dictionary! Continue without dictionary? [Y/N]"
        c <- getUpperChar
        when (c == 'Y') $ startGame validateNoDict
        {- if c == 'Y' then
          startGame validateNoDict
        else 
          return () -}