module Lib (Grid, setSudoku, runSudoku, solveSudoku, printGrid)
    where



import Data.List (sort)
import Data.Char (digitToInt, intToDigit)
import Data.Maybe (catMaybes, isNothing)


type Cell = Maybe Int
type Row = [Cell]
type Grid = [Row]



printGrid :: Grid -> IO ()
printGrid grid = do
    mapM_ printRowWithBorder $ zip grid [1..]
  where
    printRowWithBorder :: (Row, Int) -> IO ()
    printRowWithBorder (row, index) = do
        putStrLn $ insertVertical $ unwords $ map (maybe "#" show) row
        if (index `mod` 3 == 0 && index /= 9) then putStrLn horizontalLine
        else return ()

    insertVertical :: String -> String
    insertVertical line = start ++ " |" ++ middle ++ "| " ++ end
        where
            (start, middleend) = splitAt 5 line
            (middle, end) = splitAt 7 middleend

    horizontalLine :: String
    horizontalLine = "——————+———————+———————"



splitField :: String -> [[String]]
splitField field = map words $ lines field

setMaybes :: [[String]] -> Grid
setMaybes = map setRow
  where
    setRow :: [String] -> Row
    setRow = map setCell

    setCell :: String -> Cell
    setCell sym
      | sym == "." = Nothing
      | otherwise = Just (digitToInt $ head sym)

setSudoku :: String -> IO Grid
setSudoku nameFile = do
    contents <- readFile nameFile
    return $ setMaybes $ splitField contents



getRow :: Grid -> Int -> Row
getRow grid i = grid !! i

getColumn :: Grid -> Int -> Row
getColumn grid j = map (!! j) grid

getSquare :: Grid -> (Int, Int) -> Row
getSquare grid (i, j) =
    concatMap (take 3 . drop (3 * div j 3)) $
    take 3 $ drop (3 * div i 3) grid

notElement :: Int -> Row -> Bool
notElement num line = notElem num $ catMaybes line

checkCellIsNothing :: Grid -> (Int, Int) -> Bool
checkCellIsNothing grid (i, j) = isNothing $ grid !! i !! j

checkCell :: Grid -> (Int, Int) -> Int -> Bool
checkCell grid (i, j) num =
    checkCellIsNothing grid (i, j) &&
    notElement num (getRow grid i) &&
    notElement num (getColumn grid j) &&
    notElement num (getSquare grid (i, j))

fillCell :: Grid -> (Int, Int) -> Int -> Grid
fillCell grid (i, j) num =
    take i grid ++
    [take j (grid !! i) ++ [Just num] ++ drop (j + 1) (grid !!  i)] ++
    drop (i + 1) grid

deleteCell ::  Grid -> (Int, Int)-> Grid
deleteCell grid (i, j) =
    take i grid ++
    [take j (grid !! i) ++ [Nothing] ++ drop (j + 1) (grid !!  i)] ++
    drop (i + 1) grid 

hasAllElemnt :: Row -> Bool
hasAllElemnt list = sort (catMaybes list) == [1..9]

checkEndGame :: Grid -> Bool
checkEndGame grid =
    all hasAllElemnt grid &&
    all (hasAllElemnt . getColumn grid) [0..8] &&
    all (hasAllElemnt . getSquare grid) ([(i, j) | i <- [0, 3, 6], j <- [0, 3, 6]])



checkInput :: String -> Bool
checkInput line =
    length line > 6 &&
    elem (line !! 1) [intToDigit i | i <- [1..9]] &&
    elem (line !! 4) [intToDigit i | i <- [1..9]] &&
    elem (line !! 7) [intToDigit i | i <- [1..9]]

getPossition :: String -> (Int, Int)
getPossition line = (digitToInt (line !! 1) - 1, digitToInt (line !! 4) - 1)

getNumber :: String -> Int
getNumber line = digitToInt $ line !! 7



runSudoku :: Grid -> IO()
runSudoku grid = do

    printGrid grid

    if checkEndGame grid then putStrLn "Finish game"
    else do
        putStrLn "What do you want:"
        putStrLn "  1) set the number (enter s)"
        putStrLn "  2) remove the number (enter r)"
        line1 <- getLine
        case line1 of
            "s" -> do 
                putStrLn "Enter the possition (i, j) and the number from 1 to 9"
                line <- getLine
                if checkInput line then do
                    if checkCell grid (getPossition line) (getNumber line) then do
                        runSudoku $ fillCell grid (getPossition line) (getNumber line)
                    else do
                        putStrLn "Error: This number cannot be in this possition"
                        runSudoku grid
                else do
                    putStrLn "Error: Invalid input"
                    runSudoku grid
            
            "r" -> do
                putStrLn "Enter the possition (i, j)"
                line <- getLine
                
                if checkInput $ line ++ " 1" then do
                    runSudoku $ deleteCell grid (getPossition line)
                else do
                    putStrLn "Error: Invalid input"
                    runSudoku grid
            
            _ -> do
                putStrLn "Error: Invalid input"
                runSudoku grid



getNothingCell :: Grid -> [(Int, Int)]
getNothingCell grid = map snd $ filter (\(bool, _) -> bool) $ zip bools arr
    where
        arr = [(i, j) | i <- [0..8], j <- [0..8]]
        bools = map (checkCellIsNothing grid) arr

getAllowedNumbers :: Grid -> [((Int, Int), [Int])]
getAllowedNumbers grid = zip coords $ map (getAllowedNumbersCoord) coords
    where
        coords = getNothingCell grid
        getAllowedNumbersCoord :: (Int, Int) -> [Int]
        getAllowedNumbersCoord coord = filter (checkCell grid coord) [1..9]


stepOfSolveSudoku :: Grid -> [((Int, Int), [Int])] -> (Grid, Bool)
stepOfSolveSudoku grid [] = (grid, True) 
stepOfSolveSudoku grid ((_, []): _) = (grid, False)
stepOfSolveSudoku grid (((i, j), (num: nums)): xs) =
    let suppGrid = (fillCell grid (i, j) num) 
    in case stepOfSolveSudoku suppGrid $ getAllowedNumbers suppGrid of
        (_, False) -> stepOfSolveSudoku grid (((i, j), nums): xs)
        cort -> cort


solveSudoku :: Grid -> IO ()
solveSudoku grid = do

    let (newGrid, bool) = stepOfSolveSudoku grid $ getAllowedNumbers grid 
    if bool then do
        putStrLn "Algorithm is finised!"
        printGrid newGrid
    else
        putStrLn "This sudoku not solvable"