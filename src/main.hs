module Main (
        main
    ) where

import Board
import InitialBoard
import System.Console.ANSI
import System.IO
import Data.List.Split
import System.Environment
import Control.Concurrent

main :: IO ()
main = do
  args <- getArgs
  let width = widthArg args
  let height = heightArg args
  let initialConfig = boardConfigArg args
  board <- setupBoard width height initialConfig
  printBlock board

defaultDim :: Int
defaultDim = 10

widthArg :: [String] -> Int
widthArg (width:args) = read width :: Int
widthArg _ = defaultDim

heightArg :: [String] -> Int
heightArg (width:height:args) = read height :: Int
heightArg args = widthArg args

boardConfigArg :: [String] -> String
boardConfigArg (width:height:b:args) = b
boardConfigArg _ = ""

setupBoard :: Int -> Int -> String -> IO Board
setupBoard width height "liveLine" = return (liveLine width height)
setupBoard width height "cross" = return (cross width height)
setupBoard width height _  = randomBoard width height

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
