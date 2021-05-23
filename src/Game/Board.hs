module Game.Board
  ( Board,
    Line (..),
    createBoard,
    drawBoard,
    emptyLocations,
    getLocationsBy,
    setPiece,
    allLines,
    piecesOnLine,
    countPiecesOnLine,
  )
where

import Data.Composition ((.:))
import Data.List (intercalate, intersperse, transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (Maybe (..), isNothing)
import Game.Piece (Location, Piece, drawPiece)

-- | A 2D array to represent the board.
type Board = [[Maybe Piece]]

data Line = Row Int | Col Int | MajorDiag | MinorDiag deriving (Eq, Ord)

boardSize :: Int
boardSize = 3

createBoard :: Board
createBoard = replicate boardSize $ replicate boardSize Nothing

drawBoard :: Board -> String
drawBoard board = unlines $ intersperse buffer rows
  where
    rows = drawRow <$> board
    drawRow = intercalate "|" . fmap drawPiece
    buffer = intercalate "+" $ replicate boardSize "-"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth 0 y (_ : xs) = y : xs
replaceNth n y (x : xs) = x : replaceNth (n - 1) y xs

emptyLocations :: Board -> [Location]
emptyLocations = (`getLocationsBy` isNothing)

getLocationsBy :: Board -> (Maybe Piece -> Bool) -> [Location]
getLocationsBy board predicate =
  [ (r, c)
    | (r, row) <- zip [0 ..] board,
      (c, piece) <- zip [0 ..] row,
      predicate piece
  ]

setPiece :: Board -> Location -> Piece -> Board
setPiece board (r, c) piece = replaceNth r (replaceNth c (Just piece) (board !! r)) board

allLines :: [Line]
allLines =
  (Row <$> [0 .. boardSize - 1])
    ++ (Col <$> [0 .. boardSize - 1])
    ++ [MajorDiag, MinorDiag]

piecesOnLine :: Board -> Line -> [Maybe Piece]
piecesOnLine board (Row r) = board !! r
piecesOnLine board (Col c) = (!! c) <$> board
piecesOnLine board MajorDiag = zipWith (!!) board [0 ..]
piecesOnLine board MinorDiag = zipWith (!!) board [boardSize - 1, boardSize - 2 ..]

countPiecesOnLine :: Piece -> Board -> Line -> Int
countPiecesOnLine piece = length . filter (== Just piece) .: piecesOnLine
