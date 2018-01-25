module Board where

import Data.Vector as V
import Data.Vector.Split
import Data.List
-- import Data.List.Split

data Cell = Alive | Dead deriving (Eq)
instance Show Cell where
  show Alive = "#"
  show Dead = " "

type Cells = Vector Cell

type Board = Vector (Vector Cell)

-- From a list of cells, get the number that are alive.
census :: Cells -> Int
census = V.length . V.filter (== Alive)

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
cellAt b (i,j) = b ! i ! j

-- Get all adjacent cells of cell at given coordinate pair.
neighbors :: Board -> (Int,Int) -> Cells
neighbors b = V.map (cellAt b) . legal . V.fromList . positions
  where
    legal = V.filter (\(i,j) -> inHeight i && inWidth j)
    inWidth k = k >= 0 && k <= V.length (V.head b) - 1
    inHeight k = k >= 0 && k <= V.length b - 1
    moves = [(+0),(+(-1)),(+1)]
    positions (i,j) = Data.List.tail ((,) <$> (moves <*> [i]) <*> (moves <*> [j])) -- tail to exclude the cell itself which will be at the V.head

fateAt :: Board -> (Int, Int) -> Cell
fateAt b p@(i,j) = fate (cellAt b p) (census (neighbors b p))

-- Get the next generation of the board
-- (I.e. apply 'fate' to every element on the board)
nextGen :: Board -> Board
nextGen b = V.fromList (chunksOf (V.length (V.head b)) (V.map (fateAt b) positions))
  where positions = (,) <$> V.fromList [0..V.length b -1] <*> V.fromList [0..V.length (V.head b) -1]

-- Pretty print the board.
showBoard :: Board -> String
showBoard b = intercalate "\n" (V.toList (V.map (Data.List.concatMap show) b))

toBoard :: Int -> [Cell] -> Board
toBoard width cs = V.fromList (chunksOf width (V.fromList cs))
