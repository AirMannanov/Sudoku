module Interface (interface)
    where

import Data.Maybe (catMaybes)
import Graphics.Gloss
import Lib (Grid, Row)

window :: Display
window = InWindow "Sudoku" (800, 800) (1100, 100)

background :: Color
background = white

drawNumber :: Int -> Picture
drawNumber n = scale 0.6 0.6 $ text (show n)

drawNumbers :: [Int] -> [Point] -> [Picture]
drawNumbers nums coords = moveNums $ zip (map drawNumber nums) coords
    where
        moveNums :: [(Picture, Point)] -> [Picture]
        moveNums list = 
            map (moveNum (-1)) list ++
            map (moveNum 0) list ++
            map (moveNum 1) list

        moveNum :: Float -> (Picture, Point) -> Picture
        moveNum num (pict, (x, y)) = translate (60 + 70*x) (55 + num + 70*y) pict 

drawingField :: Grid -> [Picture]
drawingField grid = drawNumbers getNumbers getCoords
    where
        getNumbers :: [Int]
        getNumbers = catMaybes $ concat $ reverse grid

        getCoords :: [Point]
        getCoords = concat $
            map (\ (i, list) -> (map (\ j -> (j, i)) list)) $
            zip [0..] $ 
            map (suppFun) $ reverse grid

        suppFun :: Row -> [Float]
        suppFun row =  map (\ (x, _) -> x) $ filter (\ (_, cell) -> cell /= Nothing) $ zip [0..] row

drawingGrid :: [Picture]
drawingGrid = 
    (map line $ map (\ x -> [(x, 50), (x, 680)]) $ take 10 [50, 120..]) ++
    (map line $ map (\ x -> [(50, x), (680, x)]) $ take 10 [50, 120..]) ++
    (map line $ map (\ x -> [(x-1, 50), (x-1, 680)]) $ take 4 [50, 260..]) ++
    (map line $ map (\ x -> [(x+1, 50), (x+1, 680)]) $ take 4 [50, 260..]) ++
    (map line $ map (\ x -> [(50, x-1), (680, x-1)]) $ take 4 [50, 260..]) ++
    (map line $ map (\ x -> [(50, x+1), (680, x+1)]) $ take 4 [50, 260..])

drawing :: Grid -> Picture
drawing grid = pictures $ drawingGrid ++ drawingField grid


interface :: Grid ->  IO ()
interface grid = display window background (drawing grid)
