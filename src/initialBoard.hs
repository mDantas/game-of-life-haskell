module InitialBoard where

import Board
import System.Random
import Control.Monad

liveLine :: Int -> Int -> Board
liveLine width height = toBoard width (horizontalDeadHalf++liveLine++horizontalDeadHalf)
  where
    horizontalDeadHalf = concat (replicate ((div height 2) -1) (replicate width Dead))
    liveLine = replicate width Alive

randomBoard :: Int -> Int -> IO Board
randomBoard width height = do
  cells <- replicateM (width*height) (randomRIO (0 :: Int,1 :: Int))
  return (toBoard width (map (\j -> if j == 1 then Alive else Dead) cells))
