module Interface (interface)
    where

import Data.Maybe (catMaybes)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Const ( window, background, steps )
import Types ( Grid, Row, GameState(..) )
import Lib ( fillCell, deleteCell, checkCellIsNothing, checkCell )



drawNumber :: Int -> Picture
drawNumber n = scale 0.35 0.35 $ text (show n)


drawNumbers :: Grid -> [Int] -> [Point] -> [Picture]
drawNumbers grid nums coords = moveNums $ zip (map drawNumber nums) coords
    where
        moveNums :: [(Picture, Point)] -> [Picture]
        moveNums list =
            map (moveNum (-1)) list ++
            map (moveNum 0) list ++
            map (moveNum 1) list


        moveNum :: Float -> (Picture, Point) -> Picture
        moveNum num (pict, (x, y)) = if checkCell grid (round x, round y) then
                translate (-526 + 78*y) (293 + num - 78*x) pict
            else
                translate (-526 + 78*y) (293 + num - 78*x) $ color red pict

drawingField :: Grid -> [Picture]
drawingField grid = drawNumbers grid getNumbers getCoords
    where
        getNumbers :: [Int]
        getNumbers = concatMap catMaybes grid

        getCoords :: [Point]
        getCoords = concatMap
            (\ (i, list) -> map (\ j -> (i, j)) list) $
            zip [0..] $
            map suppFun grid

        suppFun :: Row -> [Float]
        suppFun row =  map fst $ filter (\ (_, cell) -> cell /= Nothing) $ zip [0..] row


drawingGrid :: [Picture]
drawingGrid =
    map (line . (\ x -> [(x-200, 351), (x-200, -351)])) (take 10 [-351, -273..]) ++
    map (line . (\ x -> [(351-200, x), (-351-200, x)])) (take 10 [-351, -273..])  ++
    map (line . (\ x -> [(x-1-200, 351), (x-1-200, -351)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(x+1-200, 351), (x+1-200, -351)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(351-200, x-1), (-351-200, x-1)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(351-200, x+1), (-351-200, x+1)])) (take 4 [-351, -117..])


drawButtons :: [Picture]
drawButtons =
  [
    translate 377 (-117)  $ rectangleWire 370 468,
    translate 377 (-300)  $ rectangleWire 362 94,
    translate 267 (-341) $ scale 0.8 0.8 $ text "Clear",
    translate 267 (-340) $ scale 0.8 0.8 $ text "Clear",
    translate 267 (-342) $ scale 0.8 0.8 $ text "Clear",
    translate 268 (-341) $ scale 0.8 0.8 $ text "Clear",
    translate 266 (-341) $ scale 0.8 0.8 $ text "Clear"
  ] ++
  zipWith (\(x, y) z -> translate x y $ scale 2.8 2.8 $ drawNumber z) [(x, y) | y <- [5, -117, -239], x <- [219, 341, 463]] [1..] ++
  zipWith (\(x, y) z -> translate (x + 1) y $ scale 2.8 2.8 $ drawNumber z) [(x, y) | y <- [5, -117, -239], x <- [219, 341, 463]] [1..] ++
  zipWith (\(x, y) z -> translate (x - 1) y $ scale 2.8 2.8 $ drawNumber z) [(x, y) | y <- [5, -117, -239], x <- [219, 341, 463]] [1..] ++
  zipWith (\(x, y) z -> translate x (y + 1) $ scale 2.8 2.8 $ drawNumber z) [(x, y) | y <- [5, -117, -239], x <- [219, 341, 463]] [1..] ++
  zipWith (\(x, y) z -> translate x (y - 1) $ scale 2.8 2.8 $ drawNumber z) [(x, y) | y <- [5, -117, -239], x <- [219, 341, 463]] [1..] ++
  [translate x y $ rectangleWire 118 118 | x <- [255, 377, 499], y <- [54, - 68, - 190]]

chosenCell :: (Int, Int) -> [Picture]
chosenCell coords = [translate x y $ color yellow $ rectangleSolid 77 77]
    where
        (x, y) = coords2Point coords

drawStartCell :: [(Int, Int)] -> [Picture]
drawStartCell = map ((\ (x, y) -> translate x y $ color (greyN 0.5) $ rectangleSolid 77 77) . coords2Point)


drawing :: GameState -> Picture
drawing (GameState grid startPoss _ Nothing) = pictures $ drawStartCell startPoss ++ drawingGrid ++ drawingField grid ++ drawButtons
drawing (GameState grid startPoss _ (Just coords)) = pictures $ drawStartCell startPoss ++ chosenCell coords ++ drawingGrid ++ drawingField grid ++ drawButtons


point2Coords :: Point -> (Int, Int)
point2Coords (x, y) = (abs (div (round (y - 273)) 78), div (round (x + 551)) 78)

coords2Point :: (Int, Int) -> Point
coords2Point (y, x) = (78 * fromIntegral x - 512, -(78 * fromIntegral y - 311))


zeroState :: Grid -> GameState
zeroState grid = GameState grid getStartPossition (0, 0) Nothing
    where
        getStartPossition :: [(Int, Int)]
        getStartPossition = filter (not . checkCellIsNothing grid) [(i, j) | i <- [0..8], j <- [0..8]]


getCoordsCell :: [(Int, Int)] -> Point -> Maybe (Int, Int)
getCoordsCell startCoords (x, y) =
    if notElem coords startCoords then Just coords
    else Nothing
        where
            coords = point2Coords (x, y)

clickFill :: Grid -> [(Int, Int)] -> Point -> Maybe (Int, Int) -> GameState
clickFill grid startPoss (x, y) Nothing
    | (-551 < x) && (x < 151) && (abs y < 351) = GameState grid startPoss (x, y) $ getCoordsCell startPoss (x, y)
    | otherwise = GameState grid startPoss (x, y) Nothing
clickFill grid startPoss (x, y) (Just point)
    | (-551 < x) && (x < 151) && (abs y < 351) = GameState grid startPoss (x, y) $ getCoordsCell startPoss (x, y)
    | (abs (x - 255) < 59)  && (abs (y - 54) < 59)  = GameState (fillCell grid point 1) startPoss (x, y) $ Just point
    | (abs (x - 377) < 59)  && (abs (y - 54) < 59)  = GameState (fillCell grid point 2) startPoss (x, y) $ Just point
    | (abs (x - 499) < 59)  && (abs (y - 54) < 59)  = GameState (fillCell grid point 3) startPoss (x, y) $ Just point
    | (abs (x - 255) < 59)  && (abs (y + 68) < 59)  = GameState (fillCell grid point 4) startPoss (x, y) $ Just point
    | (abs (x - 377) < 59)  && (abs (y + 68) < 59)  = GameState (fillCell grid point 5) startPoss (x, y) $ Just point
    | (abs (x - 499) < 59)  && (abs (y + 68) < 59)  = GameState (fillCell grid point 6) startPoss (x, y) $ Just point
    | (abs (x - 255) < 59)  && (abs (y + 190) < 59) = GameState (fillCell grid point 7) startPoss (x, y) $ Just point
    | (abs (x - 377) < 59)  && (abs (y + 190) < 59) = GameState (fillCell grid point 8) startPoss (x, y) $ Just point
    | (abs (x - 499) < 59)  && (abs (y + 190) < 59) = GameState (fillCell grid point 9) startPoss (x, y) $ Just point
    | (abs (x - 377) < 185) && (abs (y + 300) < 47) = GameState (deleteCell grid point) startPoss (x, y) $ Just point
    | otherwise = GameState grid startPoss (x, y) $ Just point

event :: Event -> GameState -> GameState
event (EventMotion possition) gameState = gameState {mousePossition = possition}
event (EventKey (MouseButton LeftButton) Down _ possition) (GameState grid startPoss _ coords) = clickFill grid startPoss possition coords
event _ gameState = gameState

update :: Float -> GameState -> GameState
update _ gameState = gameState

interface :: Grid ->  IO ()
interface grid = play window background steps (zeroState grid) drawing event update