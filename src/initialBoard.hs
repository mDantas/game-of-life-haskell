module InitialBoard where

import Board
import System.Random
import Control.Monad

liveLine :: Int -> Int -> Board
liveLine width height = toBoard width (horizontalDeadHalf++alive++horizontalDeadHalf)
  where
    horizontalDeadHalf = concat (replicate ((div height 2) -1) (replicate width Dead))
    alive = replicate width Alive

cross :: Int -> Int -> Board
cross width height = toBoard width (horizontalSplitHalf++alive++horizontalSplitHalf)
  where
    horizontalSplitHalf = concat (replicate ((div height 2) -1) (halfRowDead++midAlive++halfRowDead))
    halfRowDead
      | mod width 2 == 1 = (replicate (div width 2) Dead)
      | otherwise = (replicate ((div width 2)-1) Dead)
    midAlive
      | mod width 2 == 1 = [Alive]
      | otherwise = [Alive,Alive]
    alive
      | mod height 2 == 1 = replicate width Alive
      | otherwise = replicate (2*width) Alive


randomBoard :: Int -> Int -> IO Board
randomBoard width height = do
  cells <- replicateM (width*height) (randomRIO (0 :: Int,1 :: Int))
  return (toBoard width (map (\j -> if j == 1 then Alive else Dead) cells))
