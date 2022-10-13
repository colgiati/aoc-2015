import System.IO

getFloor :: [Char] -> Int
getFloor [] = 0
getFloor ('(':xs) = 1 + getFloor xs
getFloor (')':xs) = -1 + getFloor xs

getPosition :: [Char] -> Int
getPosition xs = _getPosition 0
  where
    _getPosition :: Int -> Int
    _getPosition n
      | getFloor (take n xs) < 0 = n
      | otherwise = _getPosition (n + 1)

main = do
    file <- readFile "input-1.txt"
    print (getFloor file)
    print (getPosition file)