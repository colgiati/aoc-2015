import System.IO
import Data.Bits

data Command = Command Instruction Target
    deriving(Eq, Read, Show)

data Instruction = 
    Set Value | 
    And Value Value | 
    Or Value Value | 
    LShift Value Value |
    RShift Value Value |
    Not Value
    deriving(Eq, Read, Show)

data Target = Target [Char]
    deriving(Eq, Read, Show)

data Value = Numeric Int | Literal [Char]
    deriving(Eq, Read, Show)

parseCommand :: [Char] -> Command
parseCommand = _parseCommand . ((filter (\x -> x /= [])) . splitCommand)
    where
        _parseCommand :: [[Char]] -> Command
        _parseCommand (instruction:target:[]) = Command (getInstruction instruction) (Target target)

getInstruction :: [Char] -> Instruction
getInstruction xs = _getInstruction (split xs " ")
    where
        _getInstruction :: [[Char]] -> Instruction
        _getInstruction (x:[]) = Set (getValue x)
        _getInstruction ("NOT":x:[]) = Not (getValue x)
        _getInstruction (x:"AND":y:[]) = And (getValue x) (getValue y)
        _getInstruction (x:"OR":y:[]) = Or (getValue x) (getValue y)
        _getInstruction (x:"LSHIFT":y:[]) = LShift (getValue x) (getValue y)
        _getInstruction (x:"RSHIFT":y:[]) = RShift (getValue x) (getValue y)

getValue :: [Char] -> Value
getValue x
    | isNumeric x = Numeric (read x :: Int)
    | otherwise = Literal x
            
isNumeric :: [Char] -> Bool
isNumeric [] = True
isNumeric (x:xs) = elem x "0123456789" && isNumeric xs

splitCommand :: [Char] -> [[Char]]
splitCommand xs = split xs " -> "

split :: [Char] -> [Char] -> [[Char]]
split [] sep = []
split xs sep = (getNextWord xs sep):(split (snd (splitAt ((length (getNextWord xs sep)) + (length sep)) xs)) sep)

getNextWord :: [Char] -> [Char] -> [Char]
getNextWord (x:xs) sep
    | startsWith (x:xs) sep = []
    | otherwise = x:(getNextWord xs sep)
getNextWord [] _ = []

startsWith :: [Char] -> [Char] -> Bool
startsWith _ [] = True
startsWith (x:xs) (y:sep)
    | x == y = startsWith xs sep
    | otherwise = False

-- I am sorry for this
getResult :: Command -> [Command] -> Int
getResult (Command (Set (Numeric x)) _) xs = x
getResult (Command (Set (Literal x)) _) xs = getResult (findCommand x xs) xs
getResult (Command (Not (Numeric x)) _) xs = (2 ^ 16) - 1 - x
getResult (Command (Not (Literal x)) _) xs = (2 ^ 16) - 1 - (getResult (findCommand x xs) xs)
getResult (Command (And (Numeric x) (Numeric y)) _) xs = x .&. y
getResult (Command (And (Literal x) (Numeric y)) _) xs = (getResult (findCommand x xs) xs) .&. y
getResult (Command (And (Numeric x) (Literal y)) _) xs = x .&. getResult (findCommand y xs) xs
getResult (Command (And (Literal x) (Literal y)) _) xs = getResult (findCommand x xs) xs .&. getResult (findCommand y xs) xs
getResult (Command (Or (Numeric x) (Numeric y)) _) xs = x .|. y
getResult (Command (Or (Literal x) (Numeric y)) _) xs = (getResult (findCommand x xs) xs) .|. y
getResult (Command (Or (Numeric x) (Literal y)) _) xs = x .|. getResult (findCommand y xs) xs
getResult (Command (Or (Literal x) (Literal y)) _) xs = getResult (findCommand x xs) xs .|. getResult (findCommand y xs) xs
getResult (Command (LShift (Numeric x) (Numeric y)) _) xs = (shiftL x y) `mod` (2 ^ 16)
getResult (Command (LShift (Literal x) (Numeric y)) _) xs = (shiftL (getResult (findCommand x xs) xs) y) `mod` (2 ^ 16)
getResult (Command (LShift (Numeric x) (Literal y)) _) xs = (shiftL x (getResult (findCommand y xs) xs)) `mod` (2 ^ 16)
getResult (Command (LShift (Literal x) (Literal y)) _) xs = (shiftL (getResult (findCommand x xs) xs) (getResult (findCommand y xs) xs)) `mod` (2 ^ 16)
getResult (Command (RShift (Numeric x) (Numeric y)) _) xs = (shiftR x y) `mod` (2 ^ 16)
getResult (Command (RShift (Literal x) (Numeric y)) _) xs = (shiftR (getResult (findCommand x xs) xs) y) `mod` (2 ^ 16)
getResult (Command (RShift (Numeric x) (Literal y)) _) xs = (shiftR x (getResult (findCommand y xs) xs)) `mod` (2 ^ 16)
getResult (Command (RShift (Literal x) (Literal y)) _) xs = (shiftR (getResult (findCommand x xs) xs) (getResult (findCommand y xs) xs)) `mod` (2 ^ 16)

findCommand :: [Char] -> [Command] -> Command
findCommand t (c:cs)
    | isTarget t c = c
    | otherwise = findCommand t cs

isTarget :: [Char] -> Command -> Bool
isTarget x (Command _ (Target t))
    | x == t = True
    | otherwise = False

main = do
    file <- readFile "input-7.txt"
    let commands = parseCommand <$> (lines file)
    print (getResult (findCommand "a" commands) commands)
    