module Main (main) where


import Lib (setSudoku)
import Interface (interface)
import Types (Grid)



checkInputChoiceLvl :: String -> Bool
checkInputChoiceLvl sym = elem sym ["s", "m", "h"] 

choiceLvl :: IO Grid
choiceLvl = do
    putStrLn "What level of difficulty do you want?"
    putStrLn "- simple (enter s)"
    putStrLn "- middle (enter m)"
    putStrLn "- hard   (enter h)"
    
    input <- getLine
    if checkInputChoiceLvl input then
        case input of
            "s" -> setSudoku "field/Field_simple.txt"
            "m" -> setSudoku "field/Field_middle.txt"
            "h" -> setSudoku "field/Field_hard.txt"
            _ -> choiceLvl
    else do
        putStrLn "Error: Invalid input"
        choiceLvl


-- checkInputChoicePlay :: String -> Bool
-- checkInputChoicePlay sym = elem sym ["p", "s"]

-- choicePlay :: Grid -> IO ()
-- choicePlay grid = do
--     putStrLn "What do you want?"
--     putStrLn "- Plasy by yourself (enter p)"
--     putStrLn "- See the solution  (enter s)"
    
--     input <- getLine
--     if checkInputChoicePlay input then
--         case input of
--             "p" -> runSudoku grid
--             "s" -> solveSudoku grid
--             _ -> choicePlay grid
--     else do
--         putStrLn "Error: Invalid input"
--         choicePlay grid


main :: IO ()
main = do

    -- grid <- choiceLvl

    interface
    -- printGrid grid

    -- choicePlay grid
