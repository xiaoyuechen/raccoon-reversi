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
   them sum them up for the board value. The other heristic the board value considers is the number
   of moves the opponent has, it is multiplied by a negative coefficient. The board value is used by minMax.
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
   Get the cells that should be flipped in a line
   PRE: line is a line given by the scan function
   RETURNS: the cells that should be flipped in a line
   EXAMPLES: getShouldFlip [(27, W), (28, W), (29, B)] Black == [(27, W), (28, W)]
 -}
getShouldFlip :: [Cell] -> Player -> [Cell]
getShouldFlip line player
  | isJust found = take (fromJust found) line
  | otherwise = []
  where
    found = findIndex (owns player) line

{- nextPos pos dir
   Get the next position on the board according to a direction relative to the pos
   PRE: dir is one of the 8 directions in allDirs
   RETURNS: -1 if the next pos is out of the board, otherwise the next pos
   EXAMPLES: nextPos 0 (1, 1) == 9
 -}
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

{- nextCell board pos dir
   Get the next cell on the board according to a direction relative to the pos
   PRE: pos is in range [0, 64)
        dir is one of the 8 directions in allDirs
   RETURNS: (-1, E) if the next pos is out of the board
            (could be better with Maybe, but it's too late when I found it out)
            otherwise the next cell
   EXAMPLES: nextCell initalBoard 0 (1, 1) == (9, E)
 -}
nextCell board pos dir
  | npos == (-1) = (-1, E)
  | otherwise = board !! npos
  where
    npos = nextPos pos dir

{- owns player cell
   Check if the player owns the cell
   RETURNS: if the player owns the cell
   EXAMPLES: owns Black (0, B) = True
 -}
owns Black (_, B) = True
owns White (_, W) = True
owns _ _ = False

{- empty cell
   Check if the cell is empty
   RETURNS: if the cell is empty
   EXAMPLES: empty (0, B) = False
 -}
empty :: (Int, CellState) -> Bool
empty (_, E) = True
empty _ = False

{- all Dirs
   All the possible directions on the board
   RETURNS: all the possible directions
   EXAMPLES: allDirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
 -}
allDirs :: [Direction]
allDirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

{- scan board pos dir
   Scan the board from a position at a direction and return the scanned line.
   Scan stops when it hits an empty cell or an edge of the board.
   Scan DOES NOT include the cell from which the scan starts.
   PRE: pos is in [0, 64)
        dir is one of the allDirs
   RETURNS: the scanned line
   EXAMPLES: scan initialBoard 26 (1, 0) == [(27,W),(28,B),(29,E)]
 -}
scan :: [Cell] -> Int -> Direction -> [Cell]
scan board pos dir = scanImpl board (nextCell board pos dir) dir
  where
    {- scanImpl board cell dir
       RETURNS: the scanned line
     -}
    -- VARIANT = unscanned cells on the board
    scanImpl :: [Cell] -> Cell -> Direction -> [Cell]
    scanImpl _ (-1, _) _ = []
    scanImpl _ (pos, E) _ = [(pos, E)]
    scanImpl board (pos, state) dir = (pos, state) : (scanImpl board ncell dir)
      where
        ncell = nextCell board pos dir

{- lineOption line player
   The possible position on a line for the player to make move on
   PRE: line is a line given by the scan function
   RETURNS: Just pos if a possible move exists, otherwise Nothing
   EXAMPLES: lineOption [(28,B),(29,E)] White == Just 29
 -}
lineOption :: [Cell] -> Player -> (Maybe Int)
lineOption (_ : []) _ = Nothing
lineOption line player = lineOptionImpl line player
  where
    {- lineOptionImpl line player
      The possible position on a line for the player to make move on
      RETURNS: Just pos if a possible move exists, otherwise Nothing
    -}
    -- VARIANT = length line
    lineOptionImpl :: [Cell] -> Player -> (Maybe Int)
    lineOptionImpl [] _ = Nothing
    lineOptionImpl (cell : []) _
      | empty cell = Just (fst cell)
      | otherwise = Nothing
    lineOptionImpl (cell : tail) player
      | owns player cell = Nothing
      | otherwise = lineOptionImpl tail player

{- cellOption board cell player
   The possible positions to allDirs of a cell for the player to make move on
   PRE: cell is on the board
   RETURNS: a list of possible positions to make move on
   EXAMPLES: cellOption initialBoard (27, W) White == [43, 29]
 -}
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

{- options board player
   Options the player has on the given board
   RETURNS: a list of possible positions to make move on
   EXAMPLES: options initalBoard White == [43,29,34,20]
 -}
options :: [Cell] -> Player -> [Int]
options board player =
  nub
    ( concatMap
        (\pos -> cellOption board (board !! pos) player)
        [0 .. 63]
    )

{- makeMove board move player
   Move move for the player, flip all the cells according to the rules
   PRE: move must be valid
   RETURNS: a new board with the move made
   EXAMPLES: makeMove initialBoard (Move 26) Black =
     [(0,E),(1,E),(2,E),(3,E),(4,E),(5,E),(6,E),(7,E),
      (8,E),(9,E),(10,E),(11,E),(12,E),(13,E),(14,E),(15,E),
      (16,E),(17,E),(18,E),(19,E),(20,E),(21,E),(22,E),(23,E),
      (24,E),(25,E),(26,B),(27,B),(28,B),(29,E),(30,E),(31,E),
      (32,E),(33,E),(34,E),(35,B),(36,W),(37,E),(38,E),(39,E),
      (40,E),(41,E),(42,E),(43,E),(44,E),(45,E),(46,E),(47,E),
      (48,E),(49,E),(50,E),(51,E),(52,E),(53,E),(54,E),(55,E),
      (56,E),(57,E),(58,E),(59,E),(60,E),(61,E),(62,E),(63,E)]
 -}
makeMove :: [Cell] -> Move -> Player -> [Cell]
makeMove board Pass _ = board
makeMove board (Move pos) player = put board pos player

{- minMax board player depth thisAIPlayer
   Run minmax algorithm and return the move which gives the min/max value and the value
   RETURNS: tuple of the move which gives the min/max value and the value
   EXAMPLES: minMax initialBoard White 1 White == (Move 43, -5)
 -}
-- VARIANT: depth
minMax :: [Cell] -> Player -> Int -> Player -> (Move, Int)
minMax board player depth thisAIPlayer
  | null ops = (Pass, thisAIPlayerBv)
  | depth == 0 =
    if null ops
      then (Pass, thisAIPlayerBv)
      else (Move (head ops), thisAIPlayerBv)
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
    thisAIPlayerBv = boardValue board thisAIPlayer

{- isOnEdge pos
   If the position is an edge position
   PRE: pos must be in [0, 64)
   RETURNS: if the position is an edge position
   EXAMPLES: isOnEdge 3 == True
 -}
isOnEdge :: Int -> Bool
isOnEdge pos =
  row == 0 || row == 7 || col == 0 || col == 7
  where
    row = quot pos 8
    col = pos - row * 8

{- isInCorner pos
   If the position is in corner
   PRE: pos must be in [0, 64)
   RETURNS: if the position is in corner
   EXAMPLES: isInCorner 0 == True
 -}
isInCorner :: Int -> Bool
isInCorner 0 = True
isInCorner 7 = True
isInCorner 56 = True
isInCorner 63 = True
isInCorner _ = False

{- isCloseToCorner pos
   If the position is directly adjacent to corner
   PRE: pos must be in [0, 64)
   RETURNS: if the position is directly adjacent to corner
   EXAMPLES: isCloseToCorner 1 == True
 -}
isCloseToCorner :: Int -> Bool
isCloseToCorner pos
  | pos == 1 || pos == 8 || pos == 9 = True
  | pos == 6 || pos == 15 || pos == 14 = True
  | pos == 48 || pos == 57 || pos == 49 = True
  | pos == 55 || pos == 62 || pos == 54 = True
  | otherwise = False

{- neighbourCorner pos
   Get the corner pos which is adjacent to the pos
   PRE: isCloseToCorner pos must be True
   RETURNS: the corner pos which is adjacent to the pos
   EXAMPLES: neighbourCorner 1 == 0
 -}
neighbourCorner :: Int -> Int
neighbourCorner pos
  | pos == 1 || pos == 8 || pos == 9 = 0
  | pos == 6 || pos == 15 || pos == 14 = 7
  | pos == 48 || pos == 57 || pos == 49 = 56
  | pos == 55 || pos == 62 || pos == 54 = 63

{- neighbourEdge pos
   Get the edge cell pos which is horizonrally/vertically adjacent to the pos
   PRE: col == 1 || col == 6 || row == 1 || row == 6
   RETURNS: the edge cell pos which is horizonrally/vertically adjacent to the pos
   EXAMPLES: neighbourEdge 11 == 3
 -}
neighbourEdge :: Int -> Int
neighbourEdge pos
  | col == 1 = pos - 1
  | col == 6 = pos + 1
  | row == 1 = (row - 1) * 8 + col
  | row == 6 = (row + 1) * 8 + col
  where
    row = quot pos 8
    col = pos - row * 8

{- niceValue board pos player
   Calculate the nice value of a single cell owned by the player
   PRE: cell at pos must belong to player
   RETURNS: the nice value of a single cell owned by the player
   EXAMPLES: niceValue initialBoard 27 White == 1
 -}
niceValue :: [Cell] -> Int -> Player -> Int
niceValue board pos player
  | isInCorner pos = 999
  | isCloseToCorner pos =
    let cornerCell = board !! (neighbourCorner pos)
        ownsCorner = owns player cornerCell
        emptyCorner = empty cornerCell
     in if isOnEdge pos
          then if ownsCorner then 20 else if not emptyCorner then 10 else -50
          else if ownsCorner then 10 else if not emptyCorner then 5 else -60
  | isOnEdge pos = 10
  | row == 1 || row == 6 || col == 1 || col == 6 =
    if owns player (board !! (neighbourEdge pos))
      then 10
      else 0
  | otherwise = 1
  where
    row = quot pos 8
    col = pos - row * 8

{- boardValue board player
   Calculate the board value of a board for a player
   RETURNS: the board value of a board for a player
   EXAMPLES: boardValue initialBoard White == -6
 -}
boardValue :: [Cell] -> Player -> Int
boardValue board player =
  - length (options board (opponent player)) * 2
    + foldl
      ( \acc (pos, state) ->
          if owns player (pos, state)
            then acc + niceValue board pos player
            else acc
      )
      0
      board

{- initialBoard
   Get the inital board
   RETURNS: the inital board
   EXAMPLES: initalBoard =
     [(0,E),(1,E),(2,E),(3,E),(4,E),(5,E),(6,E),(7,E),
      (8,E),(9,E),(10,E),(11,E),(12,E),(13,E),(14,E),(15,E),
      (16,E),(17,E),(18,E),(19,E),(20,E),(21,E),(22,E),(23,E),
      (24,E),(25,E),(26,E),(27,W),(28,B),(29,E),(30,E),(31,E),
      (32,E),(33,E),(34,E),(35,B),(36,W),(37,E),(38,E),(39,E),
      (40,E),(41,E),(42,E),(43,E),(44,E),(45,E),(46,E),(47,E),
      (48,E),(49,E),(50,E),(51,E),(52,E),(53,E),(54,E),(55,E),
      (56,E),(57,E),(58,E),(59,E),(60,E),(61,E),(62,E),(63,E)]
 -}
initialBoard :: [Cell]
initialBoard =
  zip
    [0 .. 63]
    ((replicate 27 E) ++ [W, B] ++ (replicate 6 E) ++ [B, W] ++ (replicate 27 E))

{- initial player
   Make a state with initial board and player
   RETURNS: a state with initial board and player
   EXAMPLE: initial White ==
    State [(0,E),(1,E),(2,E),(3,E),(4,E),(5,E),(6,E),(7,E),
            (8,E),(9,E),(10,E),(11,E),(12,E),(13,E),(14,E),(15,E),
            (16,E),(17,E),(18,E),(19,E),(20,E),(21,E),(22,E),(23,E),
            (24,E),(25,E),(26,E),(27,W),(28,B),(29,E),(30,E),(31,E),
            (32,E),(33,E),(34,E),(35,B),(36,W),(37,E),(38,E),(39,E),
            (40,E),(41,E),(42,E),(43,E),(44,E),(45,E),(46,E),(47,E),
            (48,E),(49,E),(50,E),(51,E),(52,E),(53,E),(54,E),(55,E),
            (56,E),(57,E),(58,E),(59,E),(60,E),(61,E),(62,E),(63,E)]
          White
 -}
initial :: Reversi.Player -> State
initial player = State initialBoard player

{- think state move seconds
   AI simulates the move for the opponent on the board.
   AI scans the board to find all the possible moves.
   AI runs minMax to determin the "best" move.
   AI simulates the move for itself.
   AI return its move decision with the new state.
   PRE: the state is either initial or the state think returns last time
        move must be a valid move
   RETURNS: tuple of ai's move and the new state
   EXAMPLES: think (State initialBoard White) Pass 100.0 ==
     (Move 43,State [(0,E),(1,E),(2,E),(3,E),(4,E),(5,E),(6,E),(7,E),
                     (8,E),(9,E),(10,E),(11,E),(12,E),(13,E),(14,E),(15,E),
                     (16,E),(17,E),(18,E),(19,E),(20,E),(21,E),(22,E),(23,E),
                     (24,E),(25,E),(26,E),(27,W),(28,B),(29,E),(30,E),(31,E),
                     (32,E),(33,E),(34,E),(35,W),(36,W),(37,E),(38,E),(39,E),
                     (40,E),(41,E),(42,E),(43,W),(44,E),(45,E),(46,E),(47,E),
                     (48,E),(49,E),(50,E),(51,E),(52,E),(53,E),(54,E),(55,E),
                     (56,E),(57,E),(58,E),(59,E),(60,E),(61,E),(62,E),(63,E)]
                     White)

 -}
think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
think (State board player) move _ =
  (myMove, State myMovedBoard player)
  where
    opponentMovedBoard = makeMove board move (opponent player)
    (myMove, _) = minMax opponentMovedBoard player 4 player
    myMovedBoard = makeMove opponentMovedBoard myMove player
