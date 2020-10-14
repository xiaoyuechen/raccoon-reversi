-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI (State, author, nickname, initial, think) where

import Data.List
import Data.Maybe
import Reversi

-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- (Remember to provide a brief (about 100-500 words) description of
   your implementation.)
 -}

data CellState = E | B | W
  deriving (Eq, Show)

type Cell = (Int, CellState)

-- the internal state of your AI
data State = State [Cell] Reversi.Player
  deriving (Eq, Show)

type Direction = (Int, Int)

author :: String
author = "Xiaoyue Chen"

nickname :: String
nickname = "Raccoon"

replaceAt n v (x : xs)
  | n == 0 = v : xs
  | otherwise = x : replaceAt (n -1) v xs

opponent :: Player -> Player
opponent Black = White
opponent White = Black

playerCellState :: Player -> CellState
playerCellState Black = B
playerCellState White = W

put :: [Cell] -> Int -> Player -> [Cell]
put board pos player =
  foldl (\b (pos, _) -> replaceAt pos (pos, playerCellState player) b) board shouldFlipCells
  where
    lines = map (scan board pos) allDirs
    shouldFlipCells = (pos, playerCellState player) : concatMap (\line -> getShouldFlip line player) lines

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

niceValue :: Int -> Int
niceValue pos
  | isInCorner pos = 999
  | isCloseToCorner pos = -10
  | isOnEdge pos = 10
  | row == 1 || row == 6 || col == 1 || col == 6 = 0
  | otherwise = 2
  where
    row = quot pos 8
    col = pos - row * 8

boardValue :: [Cell] -> Player -> Int
boardValue board player =
  foldl
    ( \acc (pos, state) ->
        if owns player (pos, state)
          then acc + niceValue pos
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
