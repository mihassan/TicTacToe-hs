{-# LANGUAGE RecordWildCards #-}

module AI.AlphaBeta
  ( alphaBeta,
  )
where

import Control.Monad
import Control.Monad.State
import Game.Board
  ( Board,
    Line,
    allLines,
    countPiecesOnLine,
    emptyLocations,
  )
import Game.Game (gameFinished, makeMove)
import Game.Piece (Move, otherPiece)
import Game.Player (Player (..), otherPlayer)

type Alpha = Int

type Beta = Int

type Depth = Int

evaluate :: Board -> Player -> Int
evaluate board player = sum $ evaluateLine board player <$> allLines

evaluateLine :: Board -> Player -> Line -> Int
evaluateLine board Player {..} line = absScore * signum (playersPieces - otherPieces)
  where
    playersPieces = countPiecesOnLine piece board line
    otherPieces = countPiecesOnLine (otherPiece piece) board line
    absScore = 10 ^ abs (playersPieces - otherPieces)

alphaBeta :: Board -> Player -> Alpha -> Beta -> Depth -> (Int, Maybe Move)
alphaBeta board player alpha beta depth
  | depth == 0 || gameFinished board player = (evaluate board player, Nothing)
  | otherwise = (`execState` (alpha, Nothing)) $ mapM go (emptyLocations board)
  where
    go :: Move -> State (Int, Maybe Move) ()
    go move = do
      alpha' <- gets fst
      let (score, _) = alphaBeta (makeMove board player move) (otherPlayer player) (- beta) (- alpha') (depth - 1)
      when (alpha' < beta && - score > alpha') $ do
        put (- score, Just move)
