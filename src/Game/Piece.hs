module Game.Piece
  ( Piece (..),
    Location,
    Move,
    drawPiece,
    otherPiece,
  )
where

data Piece = X | O deriving (Eq, Show, Read)

type Location = (Int, Int)

type Move = (Int, Int)

drawPiece :: Maybe Piece -> String
drawPiece Nothing = " "
drawPiece (Just x) = show x

otherPiece :: Piece -> Piece
otherPiece X = O
otherPiece O = X
