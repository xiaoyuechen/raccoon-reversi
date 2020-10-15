-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI (State, author, nickname, initial, think) where

import Data.List
import Data.Maybe
import Reversi

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- Game Logic
   The game state consists of a board and a player.
   The board's data structure is a list of cells.
   A cell has an index to identify itself and a state to show ownership
   Everytime when a move is given, makeMove is called. It takes the original board,
   the move, and the player who made the move and returns the new board.

   AI
   The AI fist scan the board to see what options it has.
   This is done by scanning towards 8 directions of every cell it owns.
   After options are known, a minMax algorithm with a depth limit is used to decide the best option.
   In the minMax algorithm, an evaluation algorithm first calculates the "nice value" of each cell,
   them sum them up as the board value. This board value is used by minMax.
   The AI call makeMove to update the board with the best option given by minMax.

   References
   The "nive value" is inspired by the region graph at: http://mnemstudio.org/game-reversi-example-2.htm
   The minMax algorithm is inspired by the course example code at:
   https://uppsala.instructure.com/courses/22500/files/1134891?module_item_id=104831
 -}

{- A cell has 3 states, E (empty), B (owned by Black) and W (owned by White)
 -}
data CellState = E | B | W
  deriving (Eq, Show)

{- A cell is a tuple. Int used to identify itself, and CellState used to show ownership
 -}
type Cell = (Int, CellState)

{- The internal state of your AI. Contains a list of cells (board) and a player (who I am)
-}
data State = State [Cell] Reversi.Player
  deriving (Eq, Show)

{- The direction for line scanning. Only have these 8 values:
   (-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)
 -}
type Direction = (Int, Int)

author :: String
author = "Xiaoyue Chen"

nickname :: String
nickname = "Raccoon"

{- replaceAt n v l
   Replace the list element at index with a value
   PRE: non empty list
   RETURNS: a new list with replaced values
   EXAMPLES: replaceAt 1 666 [0, 0, 0] == [0, 666, 0]
 -}
replaceAt n v (x : xs)
  | n == 0 = v : xs
  | otherwise = x : replaceAt (n -1) v xs

{- opponent player
   Get the opponent of the player
   RETURNS: The opponent of the player
   EXAMPLES: opponent Black == White
 -}
opponent :: Player -> Player
opponent Black = White
opponent White = Black

{- playerCellState player
   Get the CellState of the player
   RETURNS: the CellState of the player
   EXAMPLES: playerCellState Black == B
 -}
playerCellState :: Player -> CellState
playerCellState Black = B
playerCellState White = W

{- put board pos player
   Let player occupy the cell at pos, and flip the cells that should be flipped
   RETURNS: a new board after putting and flipping
   EXAMPLES: put initialBoard 26 Black ==
     [(0,E),(1,E),(2,E),(3,E),(4,E),(5,E),(6,E),(7,E),
      (8,E),(9,E),(10,E),(11,E),(12,E),(13,E),(14,E),(15,E),
      (16,E),(17,E),(18,E),(19,E),(20,E),(21,E),(22,E),(23,E),
      (24,E),(25,E),(26,B),(27,B),(28,B),(29,E),(30,E),(31,E),
      (32,E),(33,E),(34,E),(35,B),(36,W),(37,E),(38,E),(39,E),
      (40,E),(41,E),(42,E),(43,E),(44,E),(45,E),(46,E),(47,E),
      (48,E),(49,E),(50,E),(51,E),(52,E),(53,E),(54,E),(55,E),
      (56,E),(57,E),(58,E),(59,E),(60,E),(61,E),(62,E),(63,E)]
 -}
put :: [Cell] -> Int -> Player -> [Cell]
put board pos player =
  foldl (\b (pos, _) -> replaceAt pos (pos, playerCellState player) b) board shouldFlipCells
  where
    lines = map (scan board pos) allDirs
    shouldFlipCells = (pos, playerCellState player) : concatMap (\line -> getShouldFlip line player) lines

{- getShouldFlip line player

 -}
getShouldFlip :: [Cell] -> Player -> [Cell]
getShouldFlip line player
  | isJust found = take (fromJust found) line
  | otherwise = []
  where
    found = findIndex (owns player) line

nextPos :: Int -> (Int, Int) -> Int
nextPos pos (x, y)
  | 0 <= col + x
      && col + x < 8
      && 0 <= row + y
      && row + y < 8 =
    (row + y) * 8 + (col + x)
  | otherwise = -1
  where
    row = quot pos 8
    col = pos - row * 8

nextCell board pos dir
  | npos == (-1) = (-1, E)
  | otherwise = board !! npos
  where
    npos = nextPos pos dir

owns Black (_, B) = True
owns White (_, W) = True
owns _ _ = False

empty (_, E) = True
empty _ = False

dummyCell = (-1, E)

allDirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

scan :: [Cell] -> Int -> Direction -> [Cell]
scan board pos dir = scanImpl board (nextCell board pos dir) dir

scanImpl :: [Cell] -> Cell -> Direction -> [Cell]
scanImpl _ (-1, _) _ = []
scanImpl _ (pos, E) _ = [(pos, E)]
scanImpl board (pos, state) dir = (pos, state) : (scanImpl board ncell dir)
  where
    ncell = nextCell board pos dir

lineOption :: [Cell] -> Player -> (Maybe Int)
lineOption (_ : []) _ = Nothing
lineOption line player = lineOptionImpl line player

lineOptionImpl :: [Cell] -> Player -> (Maybe Int)
lineOptionImpl [] _ = Nothing
lineOptionImpl (cell : []) _
  | empty cell = Just (fst cell)
  | otherwise = Nothing
lineOptionImpl (cell : tail) player
  | owns player cell = Nothing
  | otherwise = lineOptionImpl tail player

cellOption :: [Cell] -> Cell -> Player -> [Int]
cellOption _ (_, E) _ = []
cellOption _ (_, B) White = []
cellOption _ (_, W) Black = []
cellOption board (pos, _) player =
  let lines = map (scan board pos) allDirs
   in concatMap
        ( \line ->
            let lineop = lineOption line player
             in if isJust lineop then [fromJust lineop] else []
        )
        lines

options board player =
  nub
    ( concatMap
        (\pos -> cellOption board (board !! pos) player)
        [0 .. 63]
    )

makeMove :: [Cell] -> Move -> Player -> [Cell]
makeMove board Pass _ = board
makeMove board (Move pos) player = put board pos player

minMax :: [Cell] -> Player -> Int -> Player -> (Move, Int)
minMax board player depth thisAIPlayer
  | null ops = (Pass, bv)
  | depth == 0 = (Move (head ops), bv)
  | otherwise =
    let newBoards = map (\pos -> put board pos player) ops
        values = map snd (map (\b -> minMax b (opponent player) (depth -1) thisAIPlayer) newBoards)
     in foldl1
          ( \(rm, rv) (m, v) ->
              if player == thisAIPlayer
                then if v > rv then (m, v) else (rm, rv)
                else if v < rv then (m, v) else (rm, rv)
          )
          (zip (map Move ops) values)
  where
    ops = options board player
    bv = boardValue board player

isOnEdge pos =
  row == 0 || row == 7 || col == 0 || col == 7
  where
    row = quot pos 8
    col = pos - row * 8

isInCorner 0 = True
isInCorner 7 = True
isInCorner 56 = True
isInCorner 63 = True
isInCorner _ = False

isCloseToCorner pos
  | pos == 1 || pos == 8 || pos == 9 = True
  | pos == 6 || pos == 15 || pos == 14 = True
  | pos == 48 || pos == 57 || pos == 49 = True
  | pos == 55 || pos == 62 || pos == 54 = True
  | otherwise = False

neighbourCorner pos
  | pos == 1 || pos == 8 || pos == 9 = 0
  | pos == 6 || pos == 15 || pos == 14 = 7
  | pos == 48 || pos == 57 || pos == 49 = 56
  | pos == 55 || pos == 62 || pos == 54 = 63

neighbourEdge pos
  | col == 1 = pos - 1
  | col == 6 = pos + 1
  | row == 1 = (row - 1) * 8 + col
  | row == 6 = (row + 1) * 8 + col
  where
    row = quot pos 8
    col = pos - row * 8

niceValue :: [Cell] -> Int -> Player -> Int
niceValue board pos player
  | isInCorner pos = 999
  | isCloseToCorner pos =
    if owns player (board !! (neighbourCorner pos))
      then 20
      else -20
  | isOnEdge pos = 8
  | row == 1 || row == 6 || col == 1 || col == 6 =
    if owns player (board !! (neighbourEdge pos))
      then 2
      else 0
  | otherwise = 2
  where
    row = quot pos 8
    col = pos - row * 8

boardValue :: [Cell] -> Player -> Int
boardValue board player =
  - length (options board (opponent player)) * 3
    + foldl
      ( \acc (pos, state) ->
          if owns player (pos, state)
            then acc + niceValue board pos player
            else acc
      )
      0
      board

initialBoard =
  zip
    [0 .. 63]
    ((replicate 27 E) ++ [W, B] ++ (replicate 6 E) ++ [B, W] ++ (replicate 27 E))

{- (Remember to provide a complete function specification.)
 -}
initial :: Reversi.Player -> State
initial player = State initialBoard player

{- (Remember to provide a complete function specification.)
 -}
think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
think (State board player) move seconds =
  (myMove, State myMovedBoard player)
  where
    opponentMovedBoard = makeMove board move (opponent player)
    (myMove, _) = minMax opponentMovedBoard player 4 player
    myMovedBoard = makeMove opponentMovedBoard myMove player
