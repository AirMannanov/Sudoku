module Interface (interface)
    where

import Data.Maybe (catMaybes)
import Graphics.Gloss
import Const ( window, background, steps )
import Types ( Grid, Row, GameState(..) )
import Lib ( fillCell, checkCell )
import Graphics.Gloss.Interface.Pure.Game

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
drawing (GameState grid _) = pictures $ drawingGrid ++ drawingField grid



zeroState :: Grid -> GameState
zeroState grid = GameState grid (0, 0)

getCoords :: Point -> (Int, Int)
getCoords (x, y) = (abs (div (round (y - 273)) 78), div (round (x + 351)) 78) 

clickFill :: Grid -> Point -> GameState
clickFill grid (x, y)
    | (abs x < 351) && (abs y < 351) = GameState (fillCell grid (getCoords (x, y)) 9) (x, y)
    | otherwise = GameState grid (x, y)

event :: Event -> GameState -> GameState
event (EventMotion possition) gameState = gameState {mousePossition = possition}
event (EventKey (MouseButton LeftButton) Down _ possition) (GameState grid _) = clickFill grid possition
event _ gameState = gameState

update :: Float -> GameState -> GameState
update 0.1 gameState = gameState

interface :: Grid ->  IO ()
interface grid = play window background steps (zeroState grid) drawing event update