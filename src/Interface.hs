module Interface (interface)
    where

import Data.Maybe (catMaybes)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Const ( window, background, steps )
import Types ( Grid, Row, GameState(..), GameParametrs(..) )
import Lib ( fillCell, deleteCell, checkCell )



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


drawButtonsNums :: [Picture]
drawButtonsNums =
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


drawStartCreen :: Picture
drawStartCreen = pictures $
    concatMap (\x -> [x, translate 0 1 x, translate 0 (-1) x, translate 1 0 x, translate (-1) 0 x])
    [
        translate (-475) 150 $ scale 0.6 0.6 $ text "Choose the difficulty level",
        rectangleWire 500 100,
        translate 0 (-125) $ rectangleWire 500 100,
        translate 0 (-250) $ rectangleWire 500 100,
        translate (-120) (-25) $ scale 0.6 0.6 $ text "Simple",
        translate (-110) (-155) $ scale 0.6 0.6 $ text "Middle",
        translate (-85) (-280) $ scale 0.6 0.6 $ text "Hard"
    ] ++
    (\x -> [translate 0 2 x, translate 0 (-2) x, translate 2 0 x, translate (-2) 0 x]) 
    (translate (-475) 150 $ scale 0.6 0.6 $ text "Choose the difficulty level")


drawing :: GameState -> Picture
drawing (GameState 1 (Just (GameParametrs grid startPoss _ Nothing))) =
    pictures $ drawStartCell startPoss ++ drawingGrid ++ drawingField grid ++ drawButtonsNums
drawing (GameState 1 (Just (GameParametrs grid startPoss _ (Just coords)))) =
    pictures $ drawStartCell startPoss ++ chosenCell coords ++ drawingGrid ++ drawingField grid ++ drawButtonsNums
drawing (GameState 0 _) = drawStartCreen
drawing (GameState _ _) = text "in development"


point2Coords :: Point -> (Int, Int)
point2Coords (x, y) = (abs (div (round (y - 273)) 78), div (round (x + 551)) 78)

coords2Point :: (Int, Int) -> Point
coords2Point (y, x) = (78 * fromIntegral x - 512, -(78 * fromIntegral y - 311))


-- setGameParameters :: Grid -> GameState
-- setGameParameters grid = GameState grid getStartPossition (0, 0) Nothing
--     where
--         getStartPossition :: [(Int, Int)]
--         getStartPossition = filter (not . checkCellIsNothing grid) [(i, j) | i <- [0..8], j <- [0..8]]


zeroState :: GameState
zeroState = GameState 0 Nothing

getCoordsCell :: [(Int, Int)] -> Point -> Maybe (Int, Int)
getCoordsCell startCoords (x, y) =
    if notElem coords startCoords then Just coords
    else Nothing
        where
            coords = point2Coords (x, y)

clickInGame :: Grid -> [(Int, Int)] -> Point -> Maybe (Int, Int) -> GameState
clickInGame grid startPoss (x, y) Nothing
    | (-551 < x) && (x < 151) && (abs y < 351) = shellGameState $ getCoordsCell startPoss (x, y)
    | otherwise = shellGameState Nothing
    where
        shellGameState :: Maybe (Int, Int) -> GameState
        shellGameState point = GameState 1 $ Just $ GameParametrs grid startPoss (x, y) point
clickInGame grid startPoss (x, y) (Just point)
    | (-551 < x) && (x < 151) && (abs y < 351) = shellGameState grid $ getCoordsCell startPoss (x, y)
    | (abs (x - 255) < 59)  && (abs (y - 54) < 59)  = shellGameState (fillCell grid point 1) $ Just point
    | (abs (x - 377) < 59)  && (abs (y - 54) < 59)  = shellGameState (fillCell grid point 2) $ Just point
    | (abs (x - 499) < 59)  && (abs (y - 54) < 59)  = shellGameState (fillCell grid point 3) $ Just point
    | (abs (x - 255) < 59)  && (abs (y + 68) < 59)  = shellGameState (fillCell grid point 4) $ Just point
    | (abs (x - 377) < 59)  && (abs (y + 68) < 59)  = shellGameState (fillCell grid point 5) $ Just point
    | (abs (x - 499) < 59)  && (abs (y + 68) < 59)  = shellGameState (fillCell grid point 6) $ Just point
    | (abs (x - 255) < 59)  && (abs (y + 190) < 59) = shellGameState (fillCell grid point 7) $ Just point
    | (abs (x - 377) < 59)  && (abs (y + 190) < 59) = shellGameState (fillCell grid point 8) $ Just point
    | (abs (x - 499) < 59)  && (abs (y + 190) < 59) = shellGameState (fillCell grid point 9) $ Just point
    | (abs (x - 377) < 185) && (abs (y + 300) < 47) = shellGameState (deleteCell grid point) $ Just point
    | otherwise = shellGameState grid $ Just point
    where
        shellGameState :: Grid -> Maybe (Int, Int) -> GameState
        shellGameState newGrid newPoint = GameState 1 $ Just $ GameParametrs newGrid startPoss (x, y) newPoint

event :: Event -> GameState -> GameState
event (EventMotion possition) (GameState 1 (Just gameParams)) =
    GameState 1 (Just $ gameParams {mousePossition = possition})
event (EventKey (MouseButton LeftButton) Down _ possition) (GameState 1 (Just (GameParametrs grid startPoss _ coords))) =
    clickInGame grid startPoss possition coords
event _ gState = gState


update :: Float -> GameState -> GameState
update _ gState = gState

interface :: IO ()
interface = play window background steps zeroState drawing event update