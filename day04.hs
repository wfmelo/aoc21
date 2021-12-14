module Day4 where

import Text.Printf
import qualified Data.Text as T
import Data.List.Split
import Data.List(transpose, partition)

type Seq = [Int]
type Board = [[Int]]

startSquidGame :: Seq -> [Board] -> (Int, Int)
startSquidGame seq boards = let result = squidGame seq boards in (head result, last result)

squidGame :: Seq -> [Board] -> [Int]
squidGame [] _ = []
squidGame _ [] = []
squidGame (n:seq) boards = let marked = map (mark n) boards
                               (winners, losers) = partition (check) marked
                     in map (score n) winners ++ squidGame seq losers

mark :: Int-> Board -> Board
mark n = map (map (\x -> if x == n then -1 else x))

check :: Board -> Bool
check board = let h = any (all (== -1)) board
                  v = any (all (== -1)) $ transpose board
               in h || v

score :: Int -> Board -> Int
score n board = n * (sum $ map (sum . filter (> 0)) board)
                  
main :: IO ()
main = do
  input <- lines <$> readFile "day04.txt"
  let seq = map (read . T.unpack) $ T.splitOn "," (T.pack $ head input)
      boards = chunksOf 5 . map (map read . words) . filter (/="") $ tail input
      result =  startSquidGame seq boards
  printf "Part1: %d\n" $ fst result
  printf "Part2: %d\n" $ snd result
