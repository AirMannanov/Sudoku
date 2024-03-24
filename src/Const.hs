module Const (
    window, 
    background
    ) where

import Graphics.Gloss


window :: Display
window = InWindow "Sudoku" (800, 800) (1100, 100)

background :: Color
background = white
