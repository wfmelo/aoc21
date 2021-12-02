module Day02 where

import Text.Printf

part1 :: [(String, Int)] -> Int
part1 = (\(horizontal, depth) -> horizontal * depth) . calculatePath (0, 0)

calculatePath :: (Int, Int) -> [(String, Int)] -> (Int, Int)
calculatePath (h, d) [] = (h, d)
calculatePath (h, d) ((course, dist):xs)
      | course == "forward" = calculatePath (h + dist, d) xs
      | course == "down"    = calculatePath (h, d + dist) xs
      | course == "up"      = calculatePath (h, d - dist) xs

part2 :: [(String, Int)] -> Int
part2 = (\(horizontal, depth, aim) -> horizontal * depth) . calculatePathWithAim (0, 0, 0)

calculatePathWithAim :: (Int, Int, Int) -> [(String, Int)] -> (Int, Int, Int)
calculatePathWithAim (h, d, a) [] = (h, d, a)
calculatePathWithAim (h, d, a) ((course, dist):xs)
      | course == "forward" = calculatePathWithAim (h + dist, d + a * dist, a) xs
      | course == "down"    = calculatePathWithAim (h, d, a + dist) xs
      | course == "up"      = calculatePathWithAim (h, d, a - dist) xs

main :: IO ()
main = do
  input <- map (\[x, y] -> (x :: String, read $ y :: Int)) . map words . lines <$> readFile "day02.txt"
  printf "Part1: %d\n" $ part1 input
  printf "Part2: %d\n" $ part2 input
