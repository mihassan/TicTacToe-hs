{-# LANGUAGE RecordWildCards #-}

module Game.Game
  ( Result (..),
    oppositeResult,
    makeMove,
    computeResult,
    isWonBy,
    gameFinished,
  )
where

import Data.Composition ((.:))
import Data.Maybe (isJust)
import Game.Board (Board, allLines, emptyLocations, piecesOnLine, setPiece)
import Game.Piece (Move, Piece (..))
import Game.Player (Player (..), otherPlayer)

data Result = Lost | Draw | Won deriving (Eq, Ord)

oppositeResult :: Result -> Result
oppositeResult Lost = Won
oppositeResult Won = Lost
oppositeResult Draw = Draw

makeMove :: Board -> Player -> Move -> Board
makeMove board Player {..} location = setPiece board location piece

computeResult :: Board -> Player -> Maybe Result
computeResult board player
  | isWonBy board player = Just Won
  | isWonBy board (otherPlayer player) = Just Lost
  | null (emptyLocations board) = Just Draw
  | otherwise = Nothing

isWonBy :: Board -> Player -> Bool
isWonBy board Player {..} = any (all (== Just piece) . piecesOnLine board) allLines

gameFinished :: Board -> Player -> Bool
gameFinished = isJust .: computeResult
