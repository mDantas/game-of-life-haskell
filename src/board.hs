module Board where

import Data.List
import Data.List.Split

data Cell = Alive | Dead deriving (Show, Eq)
type Cells = [Cell]

type Board = [[Cell]]

census :: Cells -> Int
census = (length . filter (== Alive))

fate :: Cell -> Int -> Cell
fate Dead 3 = Alive
fate Dead _ = Dead
fate Alive nAlive
  | nAlive < 2 = Dead --underpopulation
  | nAlive > 3 = Dead --overpopulation
  | otherwise = Alive

cellAt :: Board -> (Int, Int) -> Cell
cellAt b (i,j) = b !! i !! j

neighbors :: Board -> (Int,Int) -> Cells
neighbors b = map (cellAt b) . legal . positions
  where
    legal = filter (\(i,j) -> inHeight i && inWidth j)
    inWidth k = k >= 0 && k <= (length (b !! 0)) - 1
    inHeight k = k >= 0 && k <= (length b) - 1
    moves = [(+0),(+(-1)),(+1)]
    positions (i,j) = tail ((,) <$> (moves <*> [i]) <*> (moves <*> [j])) -- tail to exclude the cell itself which will be at the head

nextGen :: Board -> Board
nextGen b = chunksOf (length (b !! 0)) [fate (cellAt b position) (census (neighbors b position)) |
  position <- (,) <$> [0..(length b)-1] <*> [0..(length (b !! 0))-1]]

showBoard :: Board -> String
showBoard b = (intercalate "\n" (map (map (\c -> if c == Alive then '#' else ' ')) b))

-- for convenience
x -: f = f x
