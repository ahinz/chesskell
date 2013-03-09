-- Main file
import Char
import Control.Applicative
import Data.Maybe
import Data.List

-- Create board and pieces

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

emptyPos :: Board -> Position -> Bool
emptyPos b = isNothing . pieceAtPosition b

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

onBoard :: Position -> Bool
onBoard (r,f) = r >= 0 && r < 8 && f >= 0 && f < 8

-- Gameplay
data Move = Regular PositionedPiece PositionedPiece |
            QueensideCastle | KingsideCastle

fwdBkwPart :: ([Position],[Position]) -> [[Position]]
fwdBkwPart a = [reverse $ fst a, snd a]
longestValidRange :: Board -> [Position] -> [Position]
longestValidRange board = takeWhile (emptyPos board)

diagRanges :: Position -> [[Position]]
diagRanges (rank,file) = (fwdBkwPart $ partition ((< file) . snd) allPos1) ++
                         (fwdBkwPart $ partition ((> file) . snd) allPos2)
                         where allPos1 = filter onBoard
                                         [(rank + i, file + i) | i <- [-7..7], i /= 0]
                               allPos2 = filter onBoard
                                         [(rank + i, file - i) | i <- [-7..7], i /= 0]

rankRanges :: Position -> [[Position]]
rankRanges (rank,file) = fwdBkwPart $ partition ((< file) . snd) [(rank,i) | i <- [0..7], i /= file]

fileRanges :: Position -> [[Position]]
fileRanges (rank,file) = fwdBkwPart $ partition ((< rank) . fst) [(i,file) | i <- [0..7], i /= rank]

range :: (Position -> [[Position]]) -> Board -> Position -> [Position]
range f b p = filter onBoard . concat $ map (longestValidRange board) (f p)

rangeOnFile :: Board -> Position -> [Position]
rangeOnFile board = range fileRanges board

rangeOnRank :: Board -> Position -> [Position]
rangeOnRank board = range rankRanges board

rangeOnDiag :: Board -> Position -> [Position]
rangeOnDiag board = range diagRanges board

rookMoves :: Board -> Position -> [Position]
rookMoves board p = (rangeOnFile board p) ++
                    (rangeOnRank board p)

bishopMoves :: Board -> Position -> [Position]
bishopMoves = rangeOnDiag

queenMoves :: Board -> Position -> [Position]
queenMoves board p = bishopMoves board p ++
                     rookMoves board p
