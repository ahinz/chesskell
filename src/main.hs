-- Main file
import Char

data Color = Black | White
data PieceType = Knight | Rook | Bishop | Pawn | Queen | King
data Piece = Piece Color PieceType | Empty

instance Show Color where
  show Black = "b"
  show White = "w"

instance Show PieceType where
  show Knight = "N"
  show Rook = "R"
  show Bishop = "B"
  show Pawn = "P"
  show Queen = "Q"
  show King = "K"

instance Show Piece where
  show (Piece c t) = (show c) ++ (show t)
  show Empty = "  "

type Board = [[Piece]]
type Position = (Int, Int)

parsePosition :: String -> (Int, Int)
parsePosition (files:ranks:[]) = (file,rank)
                                 where file = ord files - 97
                                       rank = ord ranks - 49

createBoard :: Board
createBoard = replicate 8 $ replicate 8 Empty

printBoard :: Board -> String
printBoard b = (replicate 25 '-') ++ "\n" ++ foldl (++) "" (reverse (map printRow b))
             where
               printRow x = "|" ++ foldl (++) "" (map (\m -> (show m) ++ "|") x) ++ "\n" ++ (replicate 25 '-') ++ "\n"
zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex xs = zip [0..(length xs-1)] xs

placePiece :: Piece -> Board -> Position -> Board
placePiece p board (ridx,file) = reverse $ foldl updateFile [] $ zipWithIndex board
  where
    updateFile rows (idx, row) = if idx == file
                                 then (updateRow row) : rows
                                 else row : rows
    updateRow row = reverse $ foldl updateCell [] $ zipWithIndex row
    updateCell cells (idx, cell) = if idx == ridx
                                   then p : cells
                                   else cell : cells

baseBoard :: Board
baseBoard = foldl (\b f -> f b) createBoard [(placePawns Black 6),
                                             (placePawns White 1),
                                             placeRooks,
                                             placeKnights,
                                             placeBishops,
                                             placeKingAndQueen]
  where
    placePawns clr rw b = foldl (placePiece (Piece clr Pawn)) b (map (\x->(x,rw)) [0..7])
    placePieces clr pt c rw = (\x -> placePiece (Piece clr pt) x (c,rw)) .
                              (\x -> placePiece (Piece clr pt) x (7 - c,rw))
    placeRooks = (placePieces White Rook 0 0) . (placePieces Black Rook 0 7)
    placeKnights = (placePieces White Knight 1 0) . (placePieces Black Knight 1 7)
    placeBishops = (placePieces White Bishop 2 0) . (placePieces Black Bishop 2 7)
    placeKingAndQueen = (\b -> placePiece (Piece White King) b (4,0)) .
                        (\b -> placePiece (Piece White Queen) b (3,0)) .
                        (\b -> placePiece (Piece Black King) b (4,7)) .
                        (\b -> (placePiece (Piece Black Queen) b (3,7)))
