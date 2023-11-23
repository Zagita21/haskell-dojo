module TTT.A5 where

import Control.Monad (when)
import System.Random.Stateful (globalStdGen, uniformM)
import TTT.A1
import TTT.A2
import TTT.A3
import TTT.A4

-- Q#01
printBoard :: Board -> IO ()
printBoard = putStrLn . formatBoard 

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/ttt-logo.txt"

printLogo :: IO ()
printLogo = logo >>= putStr
  where
    logo = readFile _LOGO_PATH_

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (\ p -> if p then return X else return O )

-- Q#04
getMove :: Board -> IO Move
getMove b = ioUserString >>= worker
  where 
    ioUserString = putStrLn "Please enter your move (eg 'A1'):"  >> getLine
    worker s = case isValidMove b (stringToMove s) of
      True -> return $ stringToMove s
      _ -> putStrLn "Invalid move, please enter another move: " >> getMove b


-- Q#05
play :: Board -> Player -> IO ()
play b p =  
            when _DISPLAY_LOGO_ printLogo >>
            printBoard b >>
            putStrLn (promptPlayer p) >>
            getMove b >>= worker
              where
                worker m = case playMove p b m of
                  (InProgress, newboard) -> play newboard (switchPlayer p)
                  -- _ -> return ()
                  (gameState, newboard) ->  printBoard newboard >>
                                            putStrLn (showGameState gameState) >>
                                            putStrLn "Game over!" -- }
                                            -- return ()



-- Q#06

runTTT :: IO ()
runTTT = firstPlayer >>= play _EMPTY_BOARD_
  -- putStrLn "Not implemented... yet!"

-- Q#07

printLogoDo :: IO ()
{- printLogo = logo >>= putStr
  where
    logo = readFile _LOGO_PATH_ -}
printLogoDo = do
  logo <- readFile _LOGO_PATH_
  putStr logo

-- Q#08
firstPlayerDo :: IO Player
-- firstPlayer = _RANDOM_BOOL_ >>= (\ p -> if p then return X else return O )
firstPlayerDo = do
  t <- _RANDOM_BOOL_
  return (if t then X else O)

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo b = do
  move_str <- getLine
  let move = stringToMove move_str
  if isValidMove b move then
    -- do
      return move
  else
    do
      putStrLn "Invalid move, please try again: "
      getMoveDo b

-- Q#10
playDo :: Board -> Player -> IO ()
playDo b p = do
  when _DISPLAY_LOGO_ printLogo
  printBoard b 
  putStrLn $ promptPlayer p 
  move <- getMove b 
  let (gameState, newboard) = playMove p b move 
  case (gameState, newboard) of
    (InProgress, _) -> playDo newboard (switchPlayer p)
    _ -> do
          printBoard newboard
          putStrLn (showGameState gameState)
          putStrLn "Game over!"