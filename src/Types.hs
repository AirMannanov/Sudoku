module Types (Cell, Row, Grid, GameState(..), GameParametrs(..))
    where


type Cell = Maybe Int
type Row = [Cell]
type Grid = [Row]

data GameParametrs = GameParametrs {
        gameStateGrid :: Grid,
        startPossitions :: [(Int, Int)],
        coordinates :: Maybe (Int, Int)
}

data GameState = GameState { 
        gameState :: Int,
        gameParametrs :: Maybe GameParametrs,
        mousePossition :: (Float, Float)
    }