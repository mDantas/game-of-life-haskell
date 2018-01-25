module Board where

import Data.List
import Data.List.Split

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
  show Alive = "#"
  show Dead = " "

type Cells = [Cell]

type Board = [[Cell]]

-- From a list of cells, get the number that are alive.
census :: Cells -> Int
census = length . filter (== Alive)

-- The basic rules for Conway's Game of Life
fate :: Cell -> Int -> Cell
fate Dead 3 = Alive
fate Dead _ = Dead
fate Alive nAlive
  | nAlive < 2 = Dead --underpopulation
  | nAlive > 3 = Dead --overpopulation
  | otherwise = Alive

-- Get cell at coordinates
cellAt :: Board -> (Int, Int) -> Cell
cellAt b (i,j) = b !! i !! j

-- Get all adjacent cells of cell at given coordinate pair.
neighbors :: Board -> (Int,Int) -> Cells
neighbors b = map (cellAt b) . legal . positions
  where
    legal = filter (\(i,j) -> inHeight i && inWidth j)
    inWidth k = k >= 0 && k <= length (head b) - 1
    inHeight k = k >= 0 && k <= length b - 1
    moves = [(+0),(+(-1)),(+1)]
    positions (i,j) = tail ((,) <$> (moves <*> [i]) <*> (moves <*> [j])) -- tail to exclude the cell itself which will be at the head

-- Get the next generation of the board
-- (I.e. apply 'fate' to every element on the board)
nextGen :: Board -> Board
nextGen b = chunksOf (length (head b)) [fate (cellAt b position) (census (neighbors b position)) |
  position <- (,) <$> [0..length b -1] <*> [0..length (head b) -1]]

-- Pretty print the board.
showBoard :: Board -> String
showBoard = intercalate "\n" . map (concatMap show)
