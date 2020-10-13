-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI (State, author, nickname, initial, think) where

import Data.List
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
put board pos player = replaceAt pos (pos, playerCellState player) board

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

options (State board player) = nub (concatMap (\pos -> cellOption board (board !! pos) player) [0 .. 63])

initialBoard =
  zip
    [0 .. 63]
    ((replicate 27 E) ++ [W, B] ++ (replicate 6 E) ++ [B, W] ++ (replicate 27 E))

{- (Remember to provide a complete function specification.)
 -}
initial :: Reversi.Player -> State
initial player = State initialBoard player

newState :: State -> Reversi.Move -> State
newState state Pass = state
newState (State board player) (Move pos) =
  State
    ( replaceAt
        pos
        (pos, playerCellState (otherPlayer player))
        board
    )
    player

{- (Remember to provide a complete function specification.)
 -}
think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
think (State board player) move seconds
  | length ops == 0 = (Pass, nstate)
  | otherwise =
    let putPos = fst (head ops)
     in ((Move putPos), State (put board putPos player) player)
  where
    nstate = newState (State board player) move
    ops = options (State board player)
