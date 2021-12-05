import Data.List (foldl')

main = do
  contents <- readFile "input.txt"
  let instructions = getInstructions $ lines contents
  print $ foldl' runCommand ident instructions

type Command = (String, Integer)
type Pos = (Integer, Integer)

ident :: Pos
ident = (0, 0)

getInstructions :: [String] -> [Command]
getInstructions rows = map (toInstruction . words) rows
  where
    toInstruction (a:bs) = (a, read (head bs))

runCommand :: Pos -> Command -> Pos
runCommand (hpos, depth) ("forward", amount) = (hpos + amount, depth)
runCommand (hpos, depth) ("up", amount) = (hpos, depth - amount)
runCommand (hpos, depth) ("down", amount) = (hpos, depth + amount)
runCommand _ _ = error "invalid command"
