data Cell = Alive | Dead deriving (Show, Eq)
type Cells = [Cell]

census :: Cells -> Int
census = (length . filter . (== Alive))

fate :: Cell -> Int -> Cell
fate Dead 3 = Alive
fate Dead _ = Dead
fate Alive nAlive
  | nAlive < 2 = Dead --underpopulation
  | nAlive > 3 = Dead --overpopulation
  | otherwise = Alive
