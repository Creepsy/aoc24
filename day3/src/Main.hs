module Main where
import System.Environment (getArgs)
import Text.Regex.TDFA ((=~))

type Command = [String]

main :: IO ()
main = do
    file:_ <- getArgs
    input <- parse <$> readFile file
    print $ part1 input
    print $ part2 input

parse :: String -> String
parse = id

part1 :: String -> Int
part1 input = evalMul $ input =~ mulRegex 

part2 :: String -> Int
part2 input = evalMul . snd . foldl commandFilter (True, []) $ (input =~ statementRegex :: [[String]])
    where
        statementRegex = mulRegex ++ "|" ++ doRegex ++ "|" ++ dontRegex

commandFilter :: (Bool, [Command]) -> Command -> (Bool, [Command]) 
commandFilter (_, cmds) ("do()":_) = (True, cmds)
commandFilter (_, cmds) ("don't()":_) = (False, cmds)
commandFilter (True, cmds) cmd = (True, cmd:cmds)
commandFilter b _ = b

evalMul :: [Command] -> Int
evalMul = sum . map(uncurry(*) . extractArgs)
    where 
        extractArgs [_, a, b] = (read a, read b)
        extractArgs _ = error "Invalid arguments"

mulRegex :: String
mulRegex = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

doRegex :: String
doRegex = "do\\(\\)"

dontRegex :: String
dontRegex = "don't\\(\\)"
