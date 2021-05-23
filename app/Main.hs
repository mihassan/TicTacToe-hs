{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Functor ((<&>))
import Lib
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import System.Random (Random (randomRIO))

initGame :: IO Players
initGame =
  prompt "Select your piece (X/O): " >>= \case
    Nothing -> putStrLn "Invalid choice. Enter 'X' or 'O'." >> initGame
    Just piece ->
      randomRIO (1 :: Int, 2) >>= \case
        1 -> putStrLn "You go first" >> return (me, ai)
        2 -> putStrLn "You go second" >> return (ai, me)
      where
        me = Player {piece = piece, isAI = False}
        ai = Player {piece = otherPiece piece, isAI = True}

outputBoard :: Board -> IO ()
outputBoard b = putStrLn (drawBoard b) >> putStrLn "\n"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

prompt :: Read a => String -> IO (Maybe a)
prompt s = putStr s >> getLine <&> readMaybe

getStrategy :: IO Strategy
getStrategy = do
  putStr "Avaliable strategies are: "
  putStrLn . unwords $ show <$> allStrategies
  prompt "Choose a strategy for AI: " >>= \case
    Nothing -> putStrLn "Invalid strategy." >> getStrategy
    Just strategy -> return strategy

getMove :: IO Location
getMove =
  prompt "Your move: " >>= \case
    Nothing -> putStrLn "Invalid location input." >> getMove
    Just move -> return move

runHuman :: Board -> Player -> IO Board
runHuman board player@Player {..} = do
  move <- getMove
  if move `elem` emptyLocations board
    then return $ setPiece board move piece
    else putStrLn "Enter a valid location." >> runHuman board player

gameLoop :: Strategy -> Board -> Player -> Player -> IO ()
gameLoop strategy board currPlayer@Player {..} otherPlayer = do
  let humanPlayer = if isAI then otherPlayer else currPlayer
  nextBoard <-
    if isAI
      then return $ runStrategy strategy board currPlayer
      else runHuman board currPlayer
  outputBoard nextBoard
  case computeResult nextBoard humanPlayer of
    Just Won -> putStrLn "Victory!"
    Just Lost -> putStrLn "Defeat!"
    Just Draw -> putStrLn "Draw!"
    Nothing -> gameLoop strategy nextBoard otherPlayer currPlayer

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  strategy <- getStrategy
  (p1, p2) <- initGame
  gameLoop strategy createBoard p1 p2
