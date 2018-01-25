module Main (
        main
    ) where

import System.Console.ANSI
import System.IO
import Data.List
import Data.List.Split
import System.Random

import Control.Concurrent


colorComic :: [Board -> IO ()]
colorComic = [printBlock]


main :: IO ()
main = do
  c <- (mapM (\_ -> randomRIO ((0 :: Int),(1 :: Int))) [0..2499])
  let b = chunksOf 50 (map (\j -> if j == 1 then Alive else Dead)  c)
  (mapM_ (\color_comic -> resetScreen >> color_comic) (colorComic <*> [b]))

resetScreen :: IO ()
resetScreen = clearScreen >> setSGR [Reset] >> setCursorPosition 0 0

pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 50000

printBlock :: Board -> IO ()
printBlock board = do
    clearScreen >> setCursorPosition 0 0
--    setSGR [SetColor Foreground Vivid Red]
    putStrLn (showBoard board)
    pause
    pause
    printBlock (nextGen board)


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
    legal = filter (\(i,j) -> inBounds i && inBounds j)
    inBounds k = k >= 0 && k <= (length b) - 1
    moves = [(+0),(+(-1)),(+1)]
    positions (i,j) = tail ((,) <$> (moves <*> [i]) <*> (moves <*> [j])) -- tail to exclude the cell itself which will be at the head

nextGen :: Board -> Board
nextGen b = chunksOf (length b) [fate (cellAt b position) (census (neighbors b position)) |
  position <- (,) <$> [0..(length b)-1] <*> [0..(length b)-1]]

showBoard :: Board -> String
showBoard b = (intercalate "\n" (map (map (\c -> if c == Alive then '#' else ' ')) b))

-- for convenience
x -: f = f x
