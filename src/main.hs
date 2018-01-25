module Main (
        main
    ) where

import Board
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
