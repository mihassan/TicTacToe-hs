{-# LANGUAGE RecordWildCards #-}

module AI.Strategy (Strategy, allStrategies, runStrategy) where

import AI.AlphaBeta
import AI.Minimax
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Game.Board
import Game.Game
import Game.Piece
import Game.Player

data Strategy = Minimax | AlphaBeta deriving (Eq, Show, Read, Enum, Bounded)

allStrategies :: [Strategy]
allStrategies = [minBound .. maxBound]

runStrategy :: Strategy -> Board -> Player -> Board
runStrategy Minimax board player = applyOutcome board player $ minimax board player
runStrategy AlphaBeta board player = applyOutcome board player $ alphaBeta board player (- 10000) 10000 2

applyOutcome :: Board -> Player -> (a, Maybe Move) -> Board
applyOutcome board Player {..} (_, Just move) = setPiece board move piece
