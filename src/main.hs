-- Main file
import Char
import Control.Applicative
import Data.Maybe
import Data.List

type Position = (Int,Int)

data Color = Black | White deriving (Show,Eq)
data PieceType = Knight | Rook | Bishop | Pawn | Queen | King deriving (Show,Eq)
data Piece = Piece Color PieceType deriving (Eq)
data PositionedPiece = PositionedPiece Position Piece deriving (Show,Eq)

instance Show Piece where
  show (Piece Black Knight) = "n"
  show (Piece Black Rook) = "r"
  show (Piece Black Bishop) = "b"
  show (Piece Black Pawn) = "p"
  show (Piece Black Queen) = "q"
  show (Piece Black King) = "k"
  show (Piece White p) = map toUpper $ show (Piece Black p)

type Board = [PositionedPiece]

pos :: PositionedPiece -> Position
pos (PositionedPiece p _) = p

toPos :: Piece -> Position -> PositionedPiece
toPos p px = PositionedPiece px p

piece :: PositionedPiece -> Piece
piece (PositionedPiece _ p) = p

atPos :: PositionedPiece -> Position -> Bool
atPos = (==) . pos

pieceAtPosition :: Board -> Position -> Maybe Piece
pieceAtPosition b p = piece <$> (listToMaybe $ filter (\x -> atPos x p) b)

drawPiece :: Maybe Piece -> String
drawPiece = fromMaybe "_" . (<$>) show

type Rank = Int
type File = Int

mkPiece :: Color -> Rank -> File -> PieceType -> PositionedPiece
mkPiece c r f t = PositionedPiece (r,f) (Piece c t)

board :: Board
board = (pawns Black 6) ++  (pawns White 1) ++ (row Black 7) ++ (row White 0)
  where
    pawns c r = map (toPos (Piece c Pawn)) [(r,file) | file <- [0..7]]
    row c r = [mkPiece c r 0 Rook, mkPiece c r 1 Knight, mkPiece c r 2 Bishop,
               mkPiece c r 3 Queen, mkPiece c r 4 King,
               mkPiece c r 5 Bishop, mkPiece c r 6 Knight, mkPiece c r 7 Rook]

printBoard :: Board -> String
printBoard b = board ++ "\n"
  where
    piecesOnRank r = map (drawPiece . pieceAtPosition b . (,) r) [0..7]
    rankString = foldl (++) "" . intersperse " " . piecesOnRank
    rankStrings = map rankString [0..7]
    board = foldl (++) "" $ intersperse "\n" rankStrings
