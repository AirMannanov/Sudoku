module Types (Cell, Row, Grid, GameState(..))
    where


type Cell = Maybe Int
type Row = [Cell]
type Grid = [Row]

data GameState = GameState { 
        gameStateGrid :: Grid,
        startPossitions :: [(Int, Int)],
        mousePossition :: (Float, Float),
        coordinates :: Maybe (Int, Int)
    } deriving Show