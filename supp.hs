import System.IO

main :: IO()
main = do
    
    print $ unwords $ map show [[1, 2, 3], [4, 5, 6]]