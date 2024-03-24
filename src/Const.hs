module Const (
    window, 
    background,
    steps
    ) where

import Graphics.Gloss ( Display(InWindow), Color, white )


window :: Display
window = InWindow "Sudoku" (1200, 800) (500, 100)

background :: Color
background = white

steps :: Int
steps = 10