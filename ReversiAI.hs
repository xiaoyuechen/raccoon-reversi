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

otherPlayer :: Player -> Player
otherPlayer Black = White
otherPlayer White = Black

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

lineOption :: [Cell] -> Player -> (Bool, Cell)
lineOption (_ : []) _ = (False, dummyCell)
lineOption line player = lineOptionImpl line player

lineOptionImpl :: [Cell] -> Player -> (Bool, Cell)
lineOptionImpl [] _ = (False, dummyCell)
lineOptionImpl (cell : []) _ = (empty cell, cell)
lineOptionImpl (cell : tail) player
  | owns player cell = (False, dummyCell)
  | otherwise = lineOptionImpl tail player

cellOption :: [Cell] -> Cell -> Player -> [Cell]
cellOption _ (_, E) _ = []
cellOption _ (_, B) White = []
cellOption _ (_, W) Black = []
cellOption board (pos, _) player =
  let lines = map (scan board pos) allDirs
   in concatMap
        ( \line ->
            let option = lineOption line player
             in if fst option then [snd option] else []
        )
        lines

options board player = nub (concatMap (\pos -> cellOption board (board !! pos) player) [0 .. 63])

initialBoard =
  zip
    [0 .. 63]
    ((replicate 27 E) ++ [W, B] ++ (replicate 6 E) ++ [B, W] ++ (replicate 27 E))

{- (Remember to provide a complete function specification.)
 -}
initial :: Reversi.Player -> State
initial player = State initialBoard player

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

getEdgeCells :: [Cell] -> [Cell]
getEdgeCells cells = filter (\(pos, _) -> isOnEdge pos) cells

getCornerCells :: [Cell] -> [Cell]
getCornerCells cells = filter (\(pos, _) -> isInCorner pos) cells

niceValue :: Int -> Int
niceValue pos
  | isInCorner pos = 10
  | isOnEdge pos && not (isCloseToCorner pos) = 5
  | isCloseToCorner pos = 0
  | otherwise = 2

bestOption :: [Cell] -> Cell
bestOption cells = head (sortOn (\(pos, _) -> - niceValue pos) cells)

{- (Remember to provide a complete function specification.)
 -}
think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
think (State board player) (Pass) seconds
  | length ops == 0 = (Pass, (State board player))
  | otherwise = ((Move move), (State (put board move player) player))
  where
    ops = options board player
    move = fst (bestOption ops)
think (State board player) (Move pos) seconds
  | length ops == 0 = (Pass, (State opponentMovedBoard player))
  | otherwise = ((Move move), (State (put opponentMovedBoard move player) player))
  where
    opponentMovedBoard = put board pos (otherPlayer player)
    ops = options opponentMovedBoard player
    move = fst (bestOption ops)
