
import Data.List
import Data.Maybe (maybeToList)

-- board is square 2d list of pairs - [[(number, selected (bool))]]
newtype Board = Board { boardState :: [[(Int, Bool)]] }

instance Show Board where
  show b = "Board@[\n" ++ (concatMap (\row -> "  " ++ (concatMap (\(n,s) -> (if s then "!" else ".") ++ (show n) ++ ",") row) ++ "\n") (boardState b)) ++ "]"

-- e.g:
exampleBoard :: Board
exampleBoard = Board [[ (22,False), (13,False), (17, True)],
                      [ ( 8,False), ( 2, True), (23,False)],
                      [ (21, True), ( 9,False), (14, True)]]

winningBoard :: Board -> Bool
winningBoard board = win (boardState board) || (win . transpose) (boardState board)
  where
    win board = any (\row -> all ((==) True) (map snd row)) board

setCalledNumber :: Int -> Board -> Board
setCalledNumber num board = Board $ map setNum (boardState board)
  where
    setNum row = map setCell row
    setCell (n, set) = (n, if num == n then True else set)

applyNextNumber :: (Int, [Board]) -> Int -> (Int, [Board])
applyNextNumber (_, boardStates) nextNumber = (nextNumber, map (setCalledNumber nextNumber) boardStates)

allWinningBoards :: [Board] -> [Board]
allWinningBoards boards = filter winningBoard boards

boardsStateSequence :: [Board] -> [Int] -> [(Int, [Board])]
boardsStateSequence boards numbers = scanl applyNextNumber (0, boards) numbers

findWinners :: [Board] -> [Int] -> Maybe (Int, [Board])
findWinners boards numbers = winners firstWinningIteration
  where
    winners Nothing = Nothing
    winners (Just (lastNum, set)) = Just (lastNum, (allWinningBoards set))
    firstWinningIteration = find hasWinningBoard (boardsStateSequence boards numbers)
    hasWinningBoard (lastNum, set) = length (allWinningBoards set) > 0

-- sum of all unmarked numbers times the last number that was called
scoreBoards :: Maybe (Int, [Board]) -> [Int]
scoreBoards Nothing = []
scoreBoards (Just (lastNum, boards)) = map (\b -> (unmarkedNumberSum b) * lastNum) boards
  where
    unmarkedNumberSum b = sum $ map (\row -> sum (map (\(n, iss) -> if iss then n else 0) row)) (boardState b)
