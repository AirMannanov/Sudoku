module Lib (
    deleteCell,
    fillCell,
    checkCell,
    checkCellIsNothing,
    checkEndGame,
    checkCellPaste,
    solveSudoku,
    getFiledCells,
    getCountSolveSudoku
    ) where



import Data.List (sort)
import Data.Maybe (catMaybes, isNothing)
import System.IO.Unsafe ( unsafePerformIO )
import Types ( Cell, Grid, Row )



getRow :: Grid -> Int -> Row
getRow grid i = grid !! i


getColumn :: Grid -> Int -> Row
getColumn grid j = map (!! j) grid


getSquare :: Grid -> (Int, Int) -> Row
getSquare grid (i, j) =
    concatMap (take 3 . drop (3 * div j 3)) $
    take 3 $ drop (3 * div i 3) grid


checkCellIsNothing :: Grid -> (Int, Int) -> Bool
checkCellIsNothing grid (i, j) = isNothing $ grid !! i !! j


checkCell :: Grid -> (Int, Int) -> Bool
checkCell grid (i, j) =
    length (filter (== num) $ getRow grid i) <= 1 &&
    length (filter (== num) $ getColumn grid j) <= 1 &&
    length (filter (== num) $ getSquare grid (i, j)) <= 1
        where
            num = grid !! i !! j


checkCellPaste :: Grid -> (Int, Int) -> Int -> Bool
checkCellPaste grid (i, j) num =
    checkCellIsNothing grid (i, j) &&
    notElement (getRow grid i) &&
    notElement (getColumn grid j) &&
    notElement (getSquare grid (i, j))
    where
        notElement :: Row -> Bool
        notElement line = notElem num $ catMaybes line


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


checkEndGame :: Grid -> Bool
checkEndGame grid =
    all hasAllElemnt grid &&
    all (hasAllElemnt . getColumn grid) [0..8] &&
    all (hasAllElemnt . getSquare grid) ([(i, j) | i <- [0, 3, 6], j <- [0, 3, 6]])
    where
        hasAllElemnt :: Row -> Bool
        hasAllElemnt list = sort (catMaybes list) == [1..9]


getNothingCell :: Grid -> [(Int, Int)]
getNothingCell grid = map snd $ filter fst $ zip bools arr
    where
        arr = [(i, j) | i <- [0..8], j <- [0..8]]
        bools = map (checkCellIsNothing grid) arr


getAllowedNumbers :: Grid -> [((Int, Int), [Int])]
getAllowedNumbers grid = zip coords $ map getAllowedNumbersCoord coords
    where
        coords = getNothingCell grid
        getAllowedNumbersCoord :: (Int, Int) -> [Int]
        getAllowedNumbersCoord coord = filter (checkCellPaste grid coord) [1..9]


stepOfSolveSudoku :: [Grid] -> [((Int, Int), [Int])] -> ([Grid], Bool)
stepOfSolveSudoku [] _ = ([], False)
stepOfSolveSudoku grids [] = (grids, True)
stepOfSolveSudoku grids ((_, []): _) = (grids, False)
stepOfSolveSudoku (grid:grids) (((i, j), num: nums): xs) =
    let suppGrid = fillCell grid (i, j) num
    in case stepOfSolveSudoku (suppGrid:grid:grids) $ getAllowedNumbers suppGrid of
        (_, False) -> stepOfSolveSudoku (grid:grids) (((i, j), nums): xs)
        cort -> cort


solveSudoku :: Grid -> [Grid]
solveSudoku grid = fst $ stepOfSolveSudoku [grid] $ getAllowedNumbers grid


getFiledCells :: Grid -> [(Int, Int)]
getFiledCells grid = filter (\coords -> not $ checkCellIsNothing grid coords) [(i, j) | i <- [0..8], j <- [0..8]]


stepOfSolveSudokuCount :: Grid -> [((Int, Int), [Int])] -> Int -> (Grid, Bool, Int)
stepOfSolveSudokuCount grid [] count = (grid, True, count + 1) 
stepOfSolveSudokuCount grid ((_, []): _) count = (grid, False, count)
stepOfSolveSudokuCount grid (((i, j), num: nums): xs) count =
    let newGrid = fillCell grid (i, j) num 
    in case stepOfSolveSudokuCount newGrid (getAllowedNumbers newGrid) count of
        (_, False, newCount) -> stepOfSolveSudokuCount grid (((i, j), nums): xs) newCount
        (_, True,  newCount) -> stepOfSolveSudokuCount grid (((i, j), nums): xs) newCount

getCountSolveSudoku :: Grid -> Int
getCountSolveSudoku grid = thd $ stepOfSolveSudokuCount grid (getAllowedNumbers grid) 0
    where
        thd :: (a, b, c) -> c
        thd (_, _, x) = x 