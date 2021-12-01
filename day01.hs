module Day01 where

import System.IO
import Text.Printf

part1 :: [Int] -> Int
part1 xs =
  case xs of
    [] -> 0
    x:[] -> 0
    x:y:xs -> if x < y then 1 + part1 (y:xs) else part1 (y:xs)

part2 :: [Int] -> Int
part2 xs = part1 . map (\(x, y, z) -> x + y + z) $ zip3 xs (tail xs) (tail (tail xs))

main :: IO ()
main = do
  input <- map read . lines <$> readFile "day01.txt"
  printf "Part 1: %d\n" $  part1 input
  printf "Part 2: %d\n" $  part2 input
