module Interface (runGame)
    where

import Data.Maybe (catMaybes, isJust)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe ( unsafePerformIO )
import Const ( window, background, steps )
import Types ( Grid, Row, GameState(..), GameParametrs(..), GameSlider(..) )
import SudokuGrid ( getSudoku )
import Lib (
    fillCell,
    deleteCell,
    checkCell,
    checkCellIsNothing,
    checkCellPaste,
    checkEndGame,
    solveSudoku
    )



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
        suppFun row =  map fst $ filter (\ (_, cell) -> isJust cell) $ zip [0..] row


drawingGrid :: [Picture]
drawingGrid =
    map (line . (\ x -> [(x-200, 351), (x-200, -351)])) (take 10 [-351, -273..]) ++
    map (line . (\ x -> [(151, x), (-551, x)])) (take 10 [-351, -273..])  ++
    map (line . (\ x -> [(x-1-200, 351), (x-1-200, -351)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(x+1-200, 351), (x+1-200, -351)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(151, x-1), (-551, x-1)])) (take 4 [-351, -117..]) ++
    map (line . (\ x -> [(151, x+1), (-551, x+1)])) (take 4 [-351, -117..])


drawButtonsNums :: Grid -> Maybe (Int, Int) -> [Picture]
drawButtonsNums grid coords =
    -- Box
    [
        translate 377 (-117)  $ rectangleWire 370 468,
        translate 377 (-300)  $ rectangleWire 362 94
    ]
    ++
    (\x -> [x, translate 0 1 x, translate 1 0 x, translate (-1) 0 x, translate 0 (-1) x ])
    (translate 267 (-341) $ scale 0.8 0.8 $ text "Clear")
    ++
    concatMap (\x -> [x,
                      translate 0 1 x,
                      translate 1 0 x,
                      translate (-1) 0 x,
                      translate 0 (-1) x ])
    (zipWith (\(x, y) z -> translate x y $ scale 2.8 2.8 $ num2Picture z)
    [(x, y) | y <- [5, -117, -239], x <- [219, 341, 463]] [1..])
    ++
    [translate x y $ rectangleWire 118 118 | x <- [255, 377, 499], y <- [54, - 68, - 190]]
    where
        num2Picture :: Int -> Picture
        num2Picture num
            | isJust coords && checkCellPaste grid (takeCoords coords) num =
                color (dark (dark green)) $ drawNumber num
            | otherwise = drawNumber num

        takeCoords :: Maybe (Int, Int) -> (Int, Int)
        takeCoords (Just coord) = coord
        takeCoords _ = (0, 0)


drawChosenCell :: Maybe (Int, Int) -> [Picture]
drawChosenCell (Just coords) = [translate x y $ color yellow $ rectangleSolid 77 77]
    where
        (x, y) = coords2Point coords
drawChosenCell Nothing = []


drawStartCell :: [(Int, Int)] -> [Picture]
drawStartCell = map ((\ (x, y) -> translate x y $ color (greyN 0.5) $ rectangleSolid 77 77)
    . coords2Point)


drawStartCreen :: Picture
drawStartCreen = pictures $
    concatMap (\x -> [x, translate 0 1 x, translate 0 (-1) x, translate 1 0 x, translate (-1) 0 x])
    [
        translate (-475) 150 $ scale 0.6 0.6 $ text "Choose the difficulty level",
        rectangleWire 500 100,
        translate 0 (-125) $ rectangleWire 500 100,
        translate 0 (-250) $ rectangleWire 500 100,
        translate (-110) (-25) $ scale 0.6 0.6 $ text "Simple",
        translate (-130) (-155) $ scale 0.6 0.6 $ text "Medium",
        translate (-85) (-280) $ scale 0.6 0.6 $ text "Hard"
    ] ++
    (\x -> [translate 0 2 x, translate 0 (-2) x, translate 2 0 x, translate (-2) 0 x])
    (translate (-475) 150 $ scale 0.6 0.6 $ text "Choose the difficulty level")


drawEndGame :: Grid -> [Picture]
drawEndGame grid
    | checkEndGame grid =
        (\x -> [x, translate 0 1 x, translate 0 (-1) x, translate 1 0 x, translate (-1) 0 x])
        (translate 200 300 $ scale 0.3 0.3 $ text "The game is over")
    | otherwise = []


drawRestart :: [Picture]
drawRestart =
        translate 377 160 (rectangleWire 370 78) :
        (\x -> [x, translate 0 1 x, translate 0 (-1) x, translate 1 0 x, translate (-1) 0 x])
        (translate 220 144 $ scale 0.45 0.45 $ text "New game")

drawSolve :: [Picture]
drawSolve =
    translate 377 242 (rectangleWire 370 78) :
    (\x -> [x, translate 0 1 x, translate 0 (-1) x, translate 1 0 x, translate (-1) 0 x])
    (translate 205 226 $ scale 0.32 0.32 $ text "View the solution")

drawSlider :: Float -> [Picture]
drawSlider x =
    [
        translate 378 242 $ rectangleWire 370 78,
        translate 377 243 $ rectangleWire 370 78,
        translate 377 242 $ rectangleWire 370 78,
        translate 377 242 $ rectangleSolid 370 2,
        translate (202 + x) 242 $ color blue $ circleSolid 10
    ]


drawing :: GameState -> Picture
drawing (GameState 1 (Just (GameParametrs grid startPoss coords)) _ _) = pictures $
    drawStartCell startPoss ++
    drawChosenCell coords ++
    drawingGrid ++
    drawingField grid ++
    drawButtonsNums grid coords ++
    drawRestart ++
    drawSolve ++
    drawEndGame grid
drawing (GameState 2 (Just (GameParametrs grid startPoss _)) _ (Just (GameSlider _ slCoord))) = pictures $
    drawStartCell startPoss ++
    drawingGrid ++
    drawingField grid ++
    drawButtonsNums grid Nothing ++
    drawRestart ++
    drawSlider slCoord ++
    drawEndGame grid
drawing (GameState 0 _ _ _) = drawStartCreen
drawing (GameState {}) = scale 0.5 0.5 $ text "in development"


point2Coords :: Point -> (Int, Int)
point2Coords (x, y) = (abs (div (round (y - 273)) 78), div (round (x + 551)) 78)

coords2Point :: (Int, Int) -> Point
coords2Point (y, x) = (78 * fromIntegral x - 512, -(78 * fromIntegral y - 311))



zeroState :: GameState
zeroState = GameState 0 Nothing (0, 0) Nothing


clickInStartScreen :: Point -> GameState
clickInStartScreen (x, y)
    | (abs x < 250) && (abs y < 50) =
        shellGameStart $ Just $ unsafePerformIO $ getSudoku 0
    | (abs x < 250) && (abs (y + 125) < 50) =
        shellGameStart $ Just $ unsafePerformIO $ getSudoku 1
    | (abs x < 250) && (abs (y + 250) < 50) =
        shellGameStart $ Just $ unsafePerformIO $ getSudoku 2
    | otherwise = shellGameStart Nothing
    where
        shellGameStart :: Maybe Grid -> GameState
        shellGameStart (Just grid) = GameState 1 (Just $ setGameParameters grid) (x, y) (Just (GameSlider (solveSudoku grid) 0))
        shellGameStart _ = GameState 0 Nothing (x, y) Nothing

        setGameParameters :: Grid -> GameParametrs
        setGameParameters grid = GameParametrs grid (getStartPossition grid) Nothing

        getStartPossition :: Grid -> [(Int, Int)]
        getStartPossition grid = filter (not . checkCellIsNothing grid)
                                [(i, j) | i <- [0..8], j <- [0..8]]



getCoordsCell :: [(Int, Int)] -> Point -> Maybe (Int, Int)
getCoordsCell startCoords (x, y) =
    if notElem coords startCoords then Just coords
    else Nothing
        where
            coords = point2Coords (x, y)


clickInGame :: Grid -> [(Int, Int)] -> Point -> Maybe (Int, Int) -> Maybe GameSlider -> GameState
clickInGame grid startPoss (x, y) Nothing gameSlider
    | (abs (x - 377) < 185) && (abs (y - 160) < 39) =
        GameState 0 Nothing (x, y) gameSlider
    | (abs (x - 377) < 185) && (abs (y - 242) < 39) =
        GameState 2 (Just $ GameParametrs (getGrid gameSlider) startPoss Nothing) (x, y) gameSlider
    | (-551 < x) && (x < 151) && (abs y < 351) = shellGameState $ getCoordsCell startPoss (x, y)
    | otherwise = shellGameState Nothing
    where
        shellGameState :: Maybe (Int, Int) -> GameState
        shellGameState point =
            GameState 1 (Just $ GameParametrs grid startPoss point) (x, y) gameSlider

        getGrid :: Maybe GameSlider -> Grid
        getGrid (Just (GameSlider grids _)) = last grids
        getGrid _ = []

clickInGame grid startPoss (x, y) (Just point) gameSlider
    | (abs (x - 377) < 185) && (abs (y - 160) < 39) =
        GameState 0 Nothing (x, y) gameSlider
    | (abs (x - 377) < 185) && (abs (y - 242) < 39) =
        GameState 2 (Just $ GameParametrs (getGrid gameSlider) startPoss Nothing) (x, y) gameSlider
    | (-551 < x) && (x < 151) && (abs y < 351)      =  shellGameState grid $ getCoordsCell startPoss (x, y)
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
        shellGameState newGrid newPoint =
            GameState 1 (Just $ GameParametrs newGrid startPoss newPoint) (x, y) gameSlider

        getGrid :: Maybe GameSlider -> Grid
        getGrid (Just (GameSlider grids _)) = last grids
        getGrid _ = []


clickInSolve :: Grid -> [(Int, Int)] -> Point -> GameSlider -> GameState
clickInSolve grid startCoords (x, y) gameSlider
    | (abs (x - 377) < 185) && (abs (y - 160) < 39) =
        GameState 0 Nothing (x, y) $ Just gameSlider
    | (abs (x - 377) < 175) && (abs (y - 242) < 39) =
        GameState 2 (setGameParameters gameSlider) (x, y) (setGameSlider gameSlider)
    | otherwise = GameState 2 (Just $ GameParametrs grid startCoords Nothing) (x, y) $ Just gameSlider
    where
        setGameSlider :: GameSlider -> Maybe GameSlider
        setGameSlider (GameSlider grids _) = Just $ GameSlider grids (x - 202)

        setGameParameters :: GameSlider -> Maybe GameParametrs
        setGameParameters (GameSlider grids _) = Just $ GameParametrs (getGrid grids) startCoords Nothing

        getGrid :: [Grid] -> Grid
        getGrid [] = []
        getGrid grids = reverse grids !! floor ((x - 202) / (350.0 / fromIntegral (length grids)))


event :: Event -> GameState -> GameState
event (EventMotion possition) gState = gState {mousePossition = possition}
event (EventKey (MouseButton LeftButton) Down _ possition)
      (GameState 0 _ _ _) =
    clickInStartScreen possition
event (EventKey (MouseButton LeftButton) Down _ possition)
      (GameState 1 (Just (GameParametrs grid startPoss coords)) _ grids) =
    clickInGame grid startPoss possition coords grids
event (EventKey (MouseButton LeftButton) Down _ possition)
      (GameState 2 (Just (GameParametrs grid startPoss _)) _ (Just gameSlider)) =
    clickInSolve grid startPoss possition gameSlider
event _ gState = gState


update :: Float -> GameState -> GameState
update _ gState = gState


runGame :: IO ()
runGame = play window background steps zeroState drawing event update