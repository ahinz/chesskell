-- Main file
import Char
import Data.Char
import Control.Applicative
import Data.Maybe
import Data.List
import Network
import IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Text.Printf
import Debug.Trace

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

posPieceAtPosition :: Board -> Position -> Maybe PositionedPiece
posPieceAtPosition b p = listToMaybe $ filter (\x -> atPos x p) b

emptyPos :: Board -> Position -> Bool
emptyPos b = isNothing . pieceAtPosition b

drawPiece :: Maybe Piece -> String
drawPiece = fromMaybe "_" . (<$>) show

type Rank = Int
type File = Int

mkPiece :: Color -> Rank -> File -> PieceType -> PositionedPiece
mkPiece c r f t = PositionedPiece (r,f) (Piece c t)

baseBoard :: Board
baseBoard = (pawns Black 6) ++  (pawns White 1) ++ (row Black 7) ++ (row White 0)
  where
    pawns c r = map (toPos (Piece c Pawn)) [(r,file) | file <- [0..7]]
    row c r = [mkPiece c r 0 Rook, mkPiece c r 1 Knight, mkPiece c r 2 Bishop,
               mkPiece c r 3 Queen, mkPiece c r 4 King,
               mkPiece c r 5 Bishop, mkPiece c r 6 Knight, mkPiece c r 7 Rook]

printBoard :: Board -> String
printBoard b = pfx ++ lne ++ board ++ "\n" ++ lne ++ pfx
  where
    piecesOnRank r = map (drawPiece . pieceAtPosition b . (,) r) [0..7]
    rankString = foldl (++) "" . intersperse " " . piecesOnRank
    rankStrings = map rankString [0..7]
    prefixRanks = map (\(a,b) -> a : " | " ++ b) $ zip (map intToDigit [1..8]) rankStrings
    board = foldl (++) "" $ intersperse "\n" prefixRanks
    lne = "--|-----------------\n"
    pfx = "  | a b c d e f g h\n"

onBoard :: Position -> Bool
onBoard (r,f) = r >= 0 && r < 8 && f >= 0 && f < 8

color :: Piece -> Color
color (Piece c _) = c

sameColor :: Board -> Color -> Position -> Bool
sameColor board c p = fromMaybe False $ (== c) <$> color <$> (pieceAtPosition board p)


-- Gameplay
fwdBkwPart :: ([Position],[Position]) -> [[Position]]
fwdBkwPart a = [reverse $ fst a, snd a]

longestValidRange :: Board -> [Position] -> [Position]
longestValidRange board [] = []
longestValidRange board [x] = [x]
longestValidRange board (x0:x1:xs) = if emptyPos board x0 then
                                       x0:(longestValidRange board (x1:xs))
                                     else
                                       [x0]

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
range f b p = filter onBoard $ concat $ map (longestValidRange b) (f p)

rangeOnFile :: Board -> Position -> [Position]
rangeOnFile board = range fileRanges board

rangeOnRank :: Board -> Position -> [Position]
rangeOnRank board = range rankRanges board

rangeOnDiag :: Board -> Position -> [Position]
rangeOnDiag board = range diagRanges board

--- Specific piece moves

rookMoves :: Board -> Color -> Position -> [Position]
rookMoves board c p = filter (not . sameColor board c) ((rangeOnFile board p) ++ (rangeOnRank board p))

bishopMoves :: Board -> Color -> Position -> [Position]
bishopMoves board c p = filter (not . sameColor board c) (rangeOnDiag board p)

queenMoves :: Board -> Color -> Position -> [Position]
queenMoves board color p = bishopMoves board color p ++
                           rookMoves board color p

kingMoves :: Board -> Color -> Position -> [Position]
kingMoves board c (r,f) =
  filter (not . sameColor board c) $
  filter onBoard [(r+1,f+1),(r+1,f),(r+1,f-1),
                  (r  ,f+1),(r  ,f),(r  ,f-1),
                  (r-1,f+1),(r-1,f),(r-1,f-1)]

op :: Color -> Color
op Black = White
op White = Black

knightMoves :: Board -> Color -> Position -> [Position]
knightMoves b clr (r,c) = filter (\p -> not (sameColor b clr p)) $
  filter onBoard
  [(r+2,c+1),(r+2,c-1),(r+1,c+2),(r+1,c-2),
   (r-2,c+1),(r-2,c-1),(r-1,c+2),(r-1,c-2)]

pawnMoves :: Board -> Color -> Position -> [Position]
pawnMoves board White (r,f) = lft ++ fwd ++ rght ++ dblrank
                              where
                                lft = if sameColor board Black (r+1,f+1) then [(r+1,f+1)] else []
                                fwd = if emptyPos board (r+1,f) then [(r+1,f)] else []
                                rght = if sameColor board Black (r+1,f-1) then [(r+1,f-1)] else []
                                dblrank = if r == 1 &&
                                             emptyPos board (r+1,f) &&
                                             emptyPos board (r+2,f) then [(r+2,f)] else []

pawnMoves board Black (r,f) = lft ++ fwd ++ rght ++ dblrank
                              where
                                lft = if sameColor board White (r-1,f+1) then [(r-1,f+1)] else []
                                fwd = if emptyPos board (r-1,f) then [(r-1,f)] else []
                                rght = if sameColor board White (r-1,f-1) then [(r-1,f-1)] else []
                                dblrank = if r == 6 &&
                                             emptyPos board (r-1,f) &&
                                             emptyPos board (r-2,f) then [(r-2,f)] else []

---

data Move = Move PositionedPiece Position |
            QueensideCastle | KingsideCastle

moves :: Board -> PositionedPiece -> [Position]
moves b (PositionedPiece p z@(Piece clr Rook)) = rookMoves b clr p
moves b (PositionedPiece p z@(Piece clr Pawn)) = pawnMoves b clr p
moves b (PositionedPiece p z@(Piece clr Knight)) = knightMoves b clr p
moves b (PositionedPiece p z@(Piece clr Bishop)) = bishopMoves b clr p
moves b (PositionedPiece p z@(Piece clr King)) = kingMoves b clr p
moves b (PositionedPiece p z@(Piece clr Queen)) = queenMoves b clr p

movePiece :: Board -> PositionedPiece -> Position -> Board
movePiece b (PositionedPiece pos p) newp = newpiece : withoutnew
                                      where
                                        withoutold = filter (\posp -> not $ atPos posp newp) b
                                        withoutnew = filter (\posp -> not $ atPos posp pos) b
                                        newpiece = PositionedPiece newp p

posToStr :: Position -> String
posToStr (r,f) = [(chr $ f + 97), intToDigit (r + 1)]

strToPos :: String -> Position
strToPos [f, r] = (-1 + digitToInt r, -97 + ord f)

query :: Board -> String -> String
query b p = fromMaybe "" $ show <$> pieceAtPosition b (strToPos p)

move :: Board -> Move -> Maybe Board
move b (Move p1 p2) = if elem p2 $ moves b p1 then
                        Just $ movePiece b p1 p2
                      else
                        Nothing

moveStr :: Board -> String -> String -> Maybe Board
moveStr b p1 p2 = do
  piece <- posPieceAtPosition b (strToPos p1)
  newBoard <- move b (Move piece (strToPos p2))
  return newBoard

movesStr :: Maybe Board -> [(String, String)] -> Maybe Board
movesStr Nothing xs = Nothing
movesStr b [] = b
movesStr (Just b) ((p1,p2) : xs) = movesStr (moveStr b p1 p2) xs

game :: Maybe Board
game = do
  b1 <- moveStr baseBoard "d2" "d4"
  b2 <- moveStr b1 "d7" "d5"
  b3 <- moveStr b2 "e2" "e4"
  b4 <- moveStr b3 "e7" "e5"
  b5 <- moveStr b4 "e4" "d5"
  return b5

game2 = fromJust $ movesStr (Just baseBoard)
        [("d2","d4"),
         ("d7","d5"),
         ("e2","e4"),
         ("e7","e5"),
         ("d8","d6")]

-- IRC
host' = "asimov.freenode.net"
host = "leguin.freenode.net"
port = PortNumber 6666

data IRCState = PreInit
              | Preamble
              | InRoom
              | Error String deriving (Show, Eq)

liftIt :: IO a -> StateT (IO IRCState) IO a
liftIt = liftIO

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

--- Just receive some info back, let's login
nick = "adambot"
chan = "#ahchess"

processNickCommand :: String -> Handle -> IRCState -> IO IRCState
processNickCommand st h s
  | "quit" `isInfixOf` st = doquit
  | otherwise = (return $ Error "Can't call me yet")
  where
    doquit = do
      write h "QUIT" ""
      return $ Error "Done"

subidx sub str = findIndex (isPrefixOf sub) $ tails str

handleLine :: String -> Handle -> IRCState -> IO IRCState
handleLine' st h PreInit = do
  write h "NICK" nick
  write h "USER" (nick++" 0 * :tutorial bot")
  write h "JOIN" chan
  hFlush h
  return Preamble
handleLine' st h Preamble
  | "#ahchess" `isInfixOf` st = return InRoom
  | "433" `isInfixOf` st = return $ Error "Nickname in use"
  | otherwise = return Preamble

handleLine' st h InRoom = fromMaybe (return InRoom) $ procNick <$> subidx (":" ++ nick ++ ":") st
  where
    procNick i = processNickCommand (stripped i st) h InRoom
    stripSpace = dropWhile isSpace
    stripped i = stripSpace . (drop (i + 2 + (length nick)))

-- log
handleLine st h prevstate = trace st (handleLine' st h prevstate)

doit = do
  conn <- connectTo host port
  listenAndReact conn (return PreInit)
  where
    listenAndReact :: Handle -> IO IRCState -> IO IRCState
    listenAndReact handle st = st >>= (\state ->
                                        case state of
                                          Error s -> return $ Error s
                                          _       -> listenAndReact
                                                     handle
                                                     ((hGetLine handle) >>= (\l -> handleLine l handle state)))
