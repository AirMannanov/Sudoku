module Types (Cell, Row, Grid, GameState(..), GameParametrs(..), GameSlider(..))
    where


type Cell = Maybe Int
type Row = [Cell]
type Grid = [Row]

data GameParametrs = GameParametrs {
        gameStateGrid :: Grid,
        startPossitions :: [(Int, Int)],
        coordinates :: Maybe (Int, Int)
    }

data GameSlider = GameSlider {
        solutionGrids :: [Grid],
        sliderPossition :: Float
    }

data GameState = GameState { 
        gameState :: Int,
        gameParametrs :: Maybe GameParametrs,
        mousePossition :: (Float, Float),
        sliderParametrs :: Maybe GameSlider
    }