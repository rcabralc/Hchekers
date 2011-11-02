module Piece (Piece(..)) where

type Location   = (Int, Int)
data Owner      = White  | Black deriving (Show)
data PieceState = Normal | Queen deriving (Show)
data Direction  = DLeft  | DRight deriving (Show)

data Piece = Piece { location :: Location,
                     owner    :: Owner,
		     state    :: PieceState } deriving (Show)

move :: Piece -> Direction -> Piece
move  piece direction = piece `moveBy` (delta piece direction)

moveBy :: Piece -> (Int, Int) -> Piece
moveBy (Piece (x,y) owner state) (dx, dy) = Piece (x+dx, y+dy) owner state

delta :: Piece -> Direction -> (Int, Int)
delta piece direction = (dx direction, dy (owner piece))
    where dx DLeft  = (-1)
	  dx DRight = 1
	  dy White  = 1
	  dy Black  = (-1)
