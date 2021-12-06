import qualified Data.Text as T
import Data.List
import Data.Maybe (maybeToList, isNothing, isJust)

main = do
  contents <- lines <$> readFile "input.txt"
  let calledNumbers = head contents
  let nums = map (\t -> (read :: String -> Int) (T.unpack t)) $ T.splitOn (T.pack ",") (T.pack calledNumbers)
  let boards = parseBoards (drop 2 contents)
  print $ (show (length calledNumbers)) ++ " Numbers"
  print $ (show (length boards)) ++ " Boards"
  let states = winningBoardSequence boards nums
  --print states
  let lastWinner = find (\st -> (length st) > 0) states
  print lastWinner
  --print res
  --let scores = scoreBoards res
  --print scores

-- board is square 2d list of pairs - [[(number, selected (bool))]]
data Board = Board { boardState :: [[(Int, Bool)]], appliedNumbers :: [Int], serial :: [Int] }
type GameState = [Board]

instance Show Board where
  show b = let
        values = (concatMap (\row -> "  " ++ (concatMap (\(n,s) -> (if s then "!" else ".") ++ (show n) ++ ",") row) ++ "\n") (boardState b)) 
        numbers = show (appliedNumbers b)
        score = if (winningBoard b) then (show (scoreBoard b)) else ""
    in "Board@[\n" ++ values ++ ", (" ++ numbers ++ "), (score: " ++ score ++ ")]"

instance Eq Board where
  (==) a b = (serial a) == (serial b)

-- e.g:
exampleBoard :: Board
exampleBoard = Board {boardState = [[ (22,False), (13,False), (17, True)],
                                    [ ( 8,False), ( 2, True), (23,False)],
                                    [ (21, True), ( 9,False), (14, True)]],
                      appliedNumbers = [],
                      serial = [14, 9, 21, 23, 2, 8, 17, 13, 22]}

winningBoard :: Board -> Bool
winningBoard board = win (boardState board) || (win . transpose) (boardState board)
  where
    win board = any (\row -> all ((==) True) (map snd row)) board

setCalledNumber :: Int -> Board -> Board
setCalledNumber num board = Board { boardState = (map setNum (boardState board)), appliedNumbers = addNum, serial = (serial board) }
  where
    addNum = num : (appliedNumbers board)
    setNum row = map setCell row
    setCell (n, set) = (n, if num == n then True else set)

applyNextNumber :: [Board] -> Int -> [Board]
applyNextNumber boardStates nextNumber = map (setCalledNumber nextNumber) boardStates

newWinningBoards :: [GameState] -> [Board] -> [Board]
newWinningBoards previousWinners boards = filter wonAndNotPrevious boards
  where
    wonAndNotPrevious :: Board -> Bool
    wonAndNotPrevious b = (winningBoard b) && (notPreviouslyWon b)
    notPreviouslyWon :: Board -> Bool
    notPreviouslyWon b = isNothing $ find (\state -> isJust (find ((==) b) state)) previousWinners


boardsStateSequence :: [Board] -> [Int] -> [GameState]
boardsStateSequence boards numbers = scanl applyNextNumber boards numbers

winningBoardSequence :: [Board] -> [Int] -> [GameState]
winningBoardSequence boards numbers = allIterations
  where
    allIterations = foldl getWinningBoards [] (boardsStateSequence boards numbers)
    getWinningBoards :: [GameState] -> GameState -> [GameState]
    getWinningBoards states set = (newWinningBoards states set) : states


-- sum of all unmarked numbers times the last number that was called
scoreBoard :: Board -> Int
scoreBoard b = (unmarkedNumberSum b) * (lastNum b)
  where
    lastNum b = head $ appliedNumbers b
    unmarkedNumberSum b = sum $ map (\row -> sum (map (\(n, iss) -> if iss then 0 else n) row)) (boardState b)

-- simplifying this for now assuming 5x5
parseBoards :: [String] -> [Board]
parseBoards input = map buildBoard boardStructures
  where
    buildBoard b = Board { boardState = b, appliedNumbers = [], serial = (buildSerial b) }
    buildSerial b = foldl' (\acc row -> foldl' (\yacc (num, _) -> num : yacc) acc row) [] b
    boardSets = map (take 5) (chunksOf 6 input)
    boardNumberSets = map (\board -> map (\r -> map (read :: String -> Int) (words r)) board) boardSets
    boardStructures = map (\bs -> map (\r -> map (\e -> (e, False)) r) bs ) boardNumberSets


-- from Data.List.Split, not in my ghc distribution right now
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []
