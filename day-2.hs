import System.IO

getTotalSurface :: [[Char]] -> Int
getTotalSurface [] = 0
getTotalSurface (x:xs) = getSuface x + getTotalSurface xs

getSuface :: [Char] -> Int
getSuface xs = computeSurface (toInt <$> (getValues xs [] []))

computeSurface :: [Int] -> Int
computeSurface (l:w:h:_) = 2 * (l * w + w * h + h * l) + getSlack l w h
computeSurface _ = 0

getSlack :: Int -> Int -> Int -> Int
getSlack l w h
    | l * w < l * h && l * w < w * h = l * w
    | l * h < w * h = l * h
    | otherwise = w * h

getTotalLength :: [[Char]] -> Int
getTotalLength [] = 0
getTotalLength (x:xs) = getLength x +  getTotalLength xs

getLength :: [Char] -> Int
getLength xs = computeLength (toInt <$> (getValues xs [] []))

computeLength :: [Int] -> Int
computeLength (l:w:h:_)
    | l > w && l > h = 2 * h + 2 * w + l * w * h
    | w > h = 2 * h + 2 * l + l * w * h
    | otherwise = 2 * l + 2 * w + l * w * h
computeLength _ = 0


getValues :: [Char] -> [[Char]] -> [Char] -> [[Char]]
getValues [] values value = (value:values)
getValues ('x':xs) values value = getValues xs (value:values) []
getValues (x:xs) values value = getValues xs values (value ++ [x])

toInt :: [Char] -> Int
toInt = \x -> read x :: Int


main = do
    file <- readFile "i.txt"
    print (getTotalSurface (lines file))
    print (getTotalLength (lines file))