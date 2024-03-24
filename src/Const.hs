module Const (
    window, 
    background,
    steps
    ) where

import Graphics.Gloss


window :: Display
window = InWindow "Sudoku" (1000, 800) (1100, 100)

background :: Color
background = white

steps :: Int
steps = 10