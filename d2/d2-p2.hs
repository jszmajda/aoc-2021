import Data.List (foldl')

main = do
  contents <- readFile "input.txt"
  let instructions = getInstructions $ lines contents
  print $ foldl' runCommand ident instructions

type Command = (String, Integer)

-- hpos, depth, aim
type Pos = (Integer, Integer, Integer)

ident :: Pos
ident = (0, 0, 0)

getInstructions :: [String] -> [Command]
getInstructions rows = map (toInstruction . words) rows
  where
    toInstruction (a:bs) = (a, read (head bs))

runCommand :: Pos -> Command -> Pos
runCommand (hpos, depth, aim) ("forward", amount) = (hpos + amount, depth + (amount * aim), aim)
runCommand (hpos, depth, aim) ("up", amount) = (hpos, depth, aim - amount)
runCommand (hpos, depth, aim) ("down", amount) = (hpos, depth, aim + amount)
runCommand _ _ = error "invalid command"
