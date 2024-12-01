module Main where
import Data.List (transpose, sort, group)
import qualified Data.Map as M (fromList, findWithDefault)
import System.Environment (getArgs)

main :: IO ()
main = do
    file:_ <- getArgs
    input <- parse <$> readFile file
    print $ part1 input
    print $ part2 input

parse :: String -> [[Int]]
parse = transpose . map (map read . words) . lines

part1 :: [[Int]] -> Int
part1 [a, b] = sum . map abs . zipWith (-) a' $ b'
    where 
        a' = sort a
        b' = sort b
part1 _ = error "Invalid input format for part 1"

part2 :: [[Int]] -> Int
part2 [a, b] = sum . map (\x -> x * M.findWithDefault 0 x occurences) $ a
    where
        occurences = M.fromList . map (\xs@(x:_) -> (x, length xs)) . group . sort $ b
part2 _ = error "Invalid input format for part 2"