module Day03 where

import Text.Printf
import Data.List
import Data.Char(digitToInt)

part1 :: [String] -> Int
part1 xs = gammaEpsilon xs '1' * gammaEpsilon xs '0'

gammaEpsilon :: [String] -> Char -> Int
gammaEpsilon report bit = sum $ zipWith (\x y -> x * 2 ^ y)
                              (reverse . map ((\x -> if 2 * x >= length report then 1 else 0)
                                       . length
                                       . filter (== bit)) $ transpose report) [0..]
                          
part2 :: [String] -> Int
part2 xs = product $ map (\x -> co2oxygenRating 0 xs x) [(<=), (>)]

co2oxygenRating :: Int -> [String] -> (Int -> Int -> Bool) -> Int
co2oxygenRating index (x:[]) predicate= foldl (\acc c -> acc * 2 + digitToInt c) 0 x
co2oxygenRating index report predicate =
                          let zero = [x | x <- report, let zero = x, x !! index == '0']
                              one  = [y | y <- report, let one = y, y !! index == '1']
          in if length zero `predicate` length one
             then co2oxygenRating (index + 1) one predicate
             else co2oxygenRating (index + 1) zero predicate

main :: IO ()
main = do
  input <- lines <$> readFile "day03.txt"
  printf "%d\n" $ part1 input
  printf "%d\n" $ part2 input
