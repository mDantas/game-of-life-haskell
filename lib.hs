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
neighbors b = map (cellAt b) . positions
  where
    moves = [(+0),(+(-1)),(+1)]
    positions (i,j) = tail ((,) <$> (moves [i]) <*> (moves [j])) -- tail to exclude the cell itself which will be at the head
