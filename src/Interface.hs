module Interface (interface)
    where

import Data.Maybe (catMaybes)
import Graphics.Gloss
import Const ( window, background, steps )
import Types ( Grid, Row, GameState(..) )
import Graphics.Gloss.Interface.Pure.Game (Event)


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
        moveNum num (pict, (x, y)) = translate (-328 + 78*x) (-333 + num + 78*y) pict

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
    map (line . (\ x -> [(x, 351), (x, -351)])) (take 10 [-351, -273..]) ++
    map (line . (\ x -> [(351, x), (-351, x)])) (take 10 [-351, -273..])  ++
    map (line . (\ x -> [(x-1, 351), (x-1, -351)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(x+1, 351), (x+1, -351)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(351, x-1), (-351, x-1)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(351, x+1), (-351, x+1)])) (take 4 [-351, -117..])

drawing :: GameState -> Picture
drawing (GameState grid) = pictures $ drawingGrid ++ drawingField grid

zeroState :: Grid -> GameState
zeroState = GameState

event :: Event -> GameState -> GameState
event _ gameState = gameState

update :: Float -> GameState -> GameState
update 0.1 gameState = gameState

interface :: Grid ->  IO ()
interface grid = play window background steps (zeroState grid) drawing event update