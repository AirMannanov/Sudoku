module Lib (
    setSudoku,
    deleteCell,
    fillCell,
    checkCell,
    checkCellIsNothing,
    checkEndGame
    ) where



import Data.List (sort)
import Data.Char (digitToInt)
import Data.Maybe (catMaybes, isNothing)
import System.IO.Unsafe ( unsafePerformIO )
import Types



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

getSudoku :: String -> IO Grid
getSudoku nameFile = do
    contents <- readFile nameFile
    return $ setMaybes $ splitField contents

setSudoku :: String -> Grid
setSudoku fileName = unsafePerformIO $ getSudoku fileName

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

-- getNothingCell :: Grid -> [(Int, Int)]
-- getNothingCell grid = map snd $ filter fst $ zip bools arr
--     where
--         arr = [(i, j) | i <- [0..8], j <- [0..8]]
--         bools = map (checkCellIsNothing grid) arr

-- getAllowedNumbers :: Grid -> [((Int, Int), [Int])]
-- getAllowedNumbers grid = zip coords $ map getAllowedNumbersCoord coords
--     where
--         coords = getNothingCell grid
--         getAllowedNumbersCoord :: (Int, Int) -> [Int]
--         getAllowedNumbersCoord coord = filter (checkCellPaste grid coord) [1..9]

-- stepOfSolveSudoku :: Grid -> [((Int, Int), [Int])] -> (Grid, Bool)
-- stepOfSolveSudoku grid [] = (grid, True)
-- stepOfSolveSudoku grid ((_, []): _) = (grid, False)
-- stepOfSolveSudoku grid (((i, j), num: nums): xs) =
--     let suppGrid = fillCell grid (i, j) num
--     in case stepOfSolveSudoku suppGrid $ getAllowedNumbers suppGrid of
--         (_, False) -> stepOfSolveSudoku grid (((i, j), nums): xs)
--         cort -> cort
