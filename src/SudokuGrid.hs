module SudokuGrid ( getSudoku )
    where

import Types (Grid)
import Data.List (transpose, foldl')
import System.Random
import System.Random.Shuffle
import Lib ( deleteCell, getCountSolveSudoku, getFiledCells )



initialGrid :: Grid
initialGrid = map (map Just)
    [
        [1, 2, 3, 4, 5, 6, 7, 8, 9], -- 0
        [4, 5, 6, 7, 8, 9, 1, 2, 3], -- 1
        [7, 8, 9, 1, 2, 3, 4, 5, 6], -- 2
        [2, 3, 4, 5, 6, 7, 8, 9, 1], -- 3
        [5, 6, 7, 8, 9, 1, 2, 3, 4], -- 4
        [8, 9, 1, 2, 3, 4, 5, 6, 7], -- 5
        [3, 4, 5, 6, 7, 8, 9, 1, 2], -- 6
        [6, 7, 8, 9, 1, 2, 3, 4, 5], -- 7
        [9, 1, 2, 3, 4, 5, 6, 7, 8]  -- 8
    ]


transposeGrid :: Grid -> Grid
transposeGrid = transpose


swapSmall :: Int -> Int -> [a] -> [a]
swapSmall i j list
    | i /= j =
        take i list ++
        [list !! j] ++
        take (j - i - 1) (drop (i + 1) list) ++
        [list !! i] ++
        drop (j + 1) list
    | otherwise = list


swapArea :: Int -> Int -> [a] -> [a]
swapArea i j list
    | i == 0 && j == 1 =
        take 3 (drop 3 list) ++
        take 3 list ++
        drop 6 list
    | i == 0 && j == 2 =
        take 3 (drop 6 list) ++
        take 3 (drop 3 list) ++
        take 3 list
    | i == 1 && j == 2 =
        take 3 list ++
        take 3 (drop 6 list) ++
        take 3 (drop 3 list)
    | otherwise = list


swapRowsSmall :: StdGen -> Grid -> Grid
swapRowsSmall gen = swapSmall i j
    where
        lst = take 2 $ randomRs (0, 2) gen
        k = (head $ randomRs (0, 2) gen) :: Int
        i = 3 * k + minimum lst :: Int
        j = 3 * k + maximum lst :: Int


swapColumnsSmall :: StdGen -> Grid -> Grid
swapColumnsSmall gen = map (swapSmall i j)
    where
        lst = take 2 $ randomRs (0, 2) gen
        k = (head $ randomRs (0, 2) gen) :: Int
        i = 3 * k + minimum lst :: Int
        j = 3 * k + maximum lst :: Int


swapRowsArea :: StdGen -> Grid -> Grid
swapRowsArea gen = swapArea i j
    where
        lst = take 2 $ randomRs (0, 2) gen
        i = minimum lst :: Int
        j = maximum lst :: Int


swapColumnsArea :: StdGen -> Grid -> Grid
swapColumnsArea gen = map (swapArea i j)
    where
        lst = take 2 $ randomRs (0, 2) gen
        i = minimum lst :: Int
        j = maximum lst :: Int


applyRandomFunction :: StdGen -> Grid -> (StdGen, Grid)
applyRandomFunction gen grid =
    case i of
        0 -> (newGen, transposeGrid grid)
        1 -> (newGen, swapRowsSmall newGen grid)
        2 -> (newGen, swapRowsArea newGen grid)
        3 -> (newGen, swapColumnsSmall newGen grid)
        4 -> (newGen, swapColumnsArea newGen grid)
        _ -> (newGen, grid)
    where
        (i, newGen) = randomR (0, 4) gen :: (Int, StdGen)


transformGrid :: StdGen -> Grid -> Grid
transformGrid gen grid = snd $ foldl (\(g, newGen) _ -> applyRandomFunction g newGen) (gen, grid) ([1..50] :: [Integer])


deleteCellSupp :: [(Int, Int)] -> Grid -> Grid
deleteCellSupp [] grid = grid
deleteCellSupp (coord:coords) grid
    | getCountSolveSudoku (deleteCell grid coord) == 1 = deleteCell grid coord
    | otherwise = deleteCellSupp coords grid


deleteRandomCell :: StdGen -> Grid -> (StdGen, Grid)
deleteRandomCell gen grid = (gen1, deleteCellSupp filedCells grid)
    where
        list = getFiledCells grid
        filedCells = shuffle' list (length list) gen
        (_, gen1) = randomR (0, length filedCells) gen :: (Int, StdGen)


deleteRandomCells :: StdGen -> Grid -> Int -> Grid
deleteRandomCells gen grid n = snd $ foldl' (\(g, newGen) _ -> deleteRandomCell g newGen) (gen, grid) [1..n]


getSudoku :: Int -> IO Grid
getSudoku lvl = do
    gen <- newStdGen
    let transformedGrid = transformGrid gen initialGrid
        finalGrid = case lvl of
            2 -> deleteRandomCells gen transformedGrid 55
            1 -> deleteRandomCells gen transformedGrid 49
            _ -> deleteRandomCells gen transformedGrid 45
       
    return finalGrid