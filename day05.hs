module Day05 where

import Data.Char(isDigit)
import Text.Printf
import qualified Data.Text as T
import Data.List(transpose, nub, foldl')
import Data.Map (fromListWith)
import Data.Map.Strict (Map)

type Segment = [Int]

overlaps :: [(Segment, Segment)] -> (Int, Int)
overlaps segments = let straight = concatMap produceDiagram $ filter isStraight segments
                        diagonal = concatMap produceDiagram segments
                    in (countOne $ countPair straight, countOne $ countPair diagonal)

countPair :: [(Int, Int)] -> Map (Int, Int) Integer
countPair diagram = fromListWith (+) [(x,1) | x <- diagram]

countOne :: Map (Int, Int) Integer -> Int
countOne = foldl' (\acc x -> if x > 1 then acc + 1 else acc) 0

produceDiagram :: (Segment, Segment) -> [(Int, Int)]
produceDiagram (x, y) = zip (range x1 x2) (range y1 y2)
        where (x1, x2, y1, y2) = (head x, head y, last x, last y)

range :: Int -> Int -> [Int]
range start end = [start, start + signum (end - start) .. end]

isStraight :: (Segment, Segment) -> Bool
isStraight (x, y) = head x == head y || last x == last y

main :: IO ()
main = do
  input <- lines <$> readFile "day05.txt"
  let lst = map (filter (/="->")) $ map words input
      digits = map (map (map (read . T.unpack))) $ map (map (T.splitOn "," . T.pack)) lst
      segments = map (\x -> (head x, last x)) digits
      (part1, part2) = overlaps segments
  printf "Part1:%d\nPart2:%d\n" part1 part2
