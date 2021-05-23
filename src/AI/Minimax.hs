module AI.Minimax
  ( minimax,
  )
where

import Game.Board (Board, emptyLocations)
import Game.Game
  ( Result,
    computeResult,
    makeMove,
    oppositeResult,
  )
import Game.Piece (Move)
import Game.Player (Player, otherPlayer)

minimax :: Board -> Player -> (Result, Maybe Move)
minimax board player = case computeResult board player of
  Just result -> (result, Nothing)
  Nothing ->
    let candidateMoves = emptyLocations board
        nextBoards = makeMove board player <$> candidateMoves
        nextPlayer = otherPlayer player
        results = fst . (`minimax` nextPlayer) <$> nextBoards
        outcomes = zip results candidateMoves
        (bestResult, bestMove) = minimum outcomes
     in (oppositeResult bestResult, Just bestMove)
