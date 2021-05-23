{-# LANGUAGE RecordWildCards #-}

module Game.Player
  ( Player (..),
    Players,
    otherPlayer,
  )
where

import Game.Piece (Piece, otherPiece)

type Players = (Player, Player)

data Player = Player
  { piece :: Piece,
    isAI :: Bool
  }
  deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer Player {..} =
  Player
    { piece = otherPiece piece,
      isAI = not isAI
    }
