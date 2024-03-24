module Interface (interface)
    where

import Data.Maybe (catMaybes)
import Graphics.Gloss
import Const ( window, background)
import Types ( Grid, Row )



drawNumber :: Int -> Picture
drawNumber n = scale 0.35 0.35 $ text (show n)

drawNumbers :: [Int] -> [Point] -> [Picture]
drawNumbers nums coords = moveNums $ zip (map drawNumber nums) coords
    where
        moveNums :: [(Picture, Point)] -> [Picture]
        moveNums list =
            map (moveNum (-1)) list ++
            map (moveNum 0) list ++
            map (moveNum 1) list

        moveNum :: Float -> (Picture, Point) -> Picture
        moveNum num (pict, (x, y)) = translate (73 + 70*x) (68 + num + 70*y) pict

drawingField :: Grid -> [Picture]
drawingField grid = drawNumbers getNumbers getCoords
    where
        getNumbers :: [Int]
        getNumbers = concatMap catMaybes (reverse grid)

        getCoords :: [Point]
        getCoords = concatMap
            (\ (i, list) -> map (\ j -> (j, i)) list) $
            zip [0..] $
            map suppFun $ reverse grid

        suppFun :: Row -> [Float]
        suppFun row =  map fst $ filter (\ (_, cell) -> cell /= Nothing) $ zip [0..] row

drawingGrid :: [Picture]
drawingGrid =
    map (line . (\ x -> [(x, 50), (x, 680)])) (take 10 [50, 120..]) ++
    map (line . (\ x -> [(50, x), (680, x)])) (take 10 [50, 120..]) ++
    map (line . (\ x -> [(x-1, 50), (x-1, 680)])) (take 4 [50, 260..]) ++
    map (line . (\ x -> [(x+1, 50), (x+1, 680)])) (take 4 [50, 260..]) ++
    map (line . (\ x -> [(50, x-1), (680, x-1)])) (take 4 [50, 260..]) ++
    map (line . (\ x -> [(50, x+1), (680, x+1)])) (take 4 [50, 260..])

drawing :: Grid -> Picture
drawing grid = pictures $ drawingGrid ++ drawingField grid


interface :: Grid ->  IO ()
interface grid = display window background (drawing grid)
