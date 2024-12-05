module Main where

import           Control.Monad
import           System.IO

type Board = [Int]

startingGame :: Board
startingGame = reverse [1 .. 5]

printGame :: Board -> IO ()
printGame board = do
  putStrLn "NIM!"
  replicateM_ 20 (putStr "-")
  putStrLn ""
  printBoard board

printBoard :: Board -> IO ()
printBoard [] = return ()
printBoard xs = go xs (length xs)
  where
    go [] _ = return ()
    go (y : ys) n = do
      putStr (show n ++ " :")
      printRow y
      go ys (n - 1)

printRow :: Int -> IO ()
printRow 0 = putStrLn ""
printRow n = do
  putStr " * "
  printRow (n - 1)

removeElements :: Board -> Int -> Int -> Board
removeElements [] _ _         = []
removeElements (x : xs) 0 mov = subElem x mov : xs
removeElements (x : xs) n mov = x : removeElements xs (n - 1) mov

subElem :: (Ord a, Num a) => a -> a -> a
subElem a b = max (a - b) 0

checkEnd :: Board -> Bool
checkEnd = all (== 0)

data PlayerTurn = One | Two
  deriving (Eq)

nextPlayer :: PlayerTurn -> PlayerTurn
nextPlayer One = Two
nextPlayer Two = One

instance Show PlayerTurn where
  show One = "Player One"
  show Two = "Player Two"

playTurn :: Board -> PlayerTurn -> IO Board
playTurn board playerTurn = do
  printBoard board
  print playerTurn
  putStrLn ""
  putStr "Which row? "
  putStrLn ""
  row <- getIntInput
  putStrLn ""
  putStr "How many? "
  putStrLn ""
  elements <- getIntInput
  putStrLn ""
  let newBoard = removeElements board (5 - row) elements
  if checkEnd newBoard
    then do
      putStrLn $ show playerTurn ++ " Wins!"
      return []
    else playTurn newBoard (nextPlayer playerTurn)

getIntInput :: IO Int
getIntInput =
  read <$> getLine

playGame :: IO ()
playGame = do
  hSetBuffering stdin LineBuffering
  let board = startingGame
  _ <- playTurn board One
  return ()

main :: IO ()
main = playGame
