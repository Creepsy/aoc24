module Main where
import System.Environment (getArgs)
import Data.Traversable (for)

type Report = [Int]

main :: IO ()
main = do
    file:_ <- getArgs
    input <- parse <$> readFile file
    print $ part1 input
    print $ part2 input

parse :: String -> [Report]
parse = map (map read . words) . lines

part1 :: [Report] -> Int
part1 = length . filter safe

safe :: Report -> Bool
safe report = all (\d -> d > 0 && d <= 3) difference || all (\d -> d < 0 && d >= -3) difference
    where
        difference = zipWith (-) (tail report) report

part2 :: [Report] -> Int
part2 = length . filter safe'

safe' :: Report -> Bool
safe' report = safe report || (any safe . removeOne $ report)

removeOne :: [a] ->  [[a]]
removeOne xs = (\i -> take i xs ++ drop (i + 1) xs) <$> [0..length xs - 1]