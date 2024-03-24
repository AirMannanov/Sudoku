module Types (Cell, Row, Grid)
    where


type Cell = Maybe Int
type Row = [Cell]
type Grid = [Row]