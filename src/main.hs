module Main (
        main
    ) where

import Board
import System.Console.ANSI
import System.IO
import Data.List.Split
import System.Random
import System.Environment
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do
  dimensions <- getArgs
  let width = read (head dimensions) :: Int
  let height = read (dimensions !! 1) :: Int
  board <- setupBoard width height
  printBlock board

setupBoard :: Int -> Int -> IO Board
setupBoard width height = do
  cells <- replicateM (width*height) (randomRIO (0 :: Int,1 :: Int))
  let board = chunksOf width (map (\j -> if j == 1 then Alive else Dead)  cells)
  return board

pause :: IO ()
pause = do
    hFlush stdout
    -- 1 second pause
    threadDelay 100000

printBlock :: Board -> IO ()
printBlock board = do
  clearScreen >> setCursorPosition 0 0
  putStrLn (showBoard board)
  pause
  printBlock (nextGen board)
