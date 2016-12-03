module Main where

import Data.Maybe

-- Data Type to represent each letter of the source
data Token = Increment
           | Decrement
           | MoveLeft
           | MoveRight
           | Output
           | Input
           | LoopL
           | LoopR
           | Comment Char
           deriving (Show)

-- Data Type to represent the main array
data Cells a = Cells [a] a [a]

-- Type of Source program after parsing
type Program = [Token]

-- Initialize the main array (infinite)
initCells :: Cells Int
initCells = Cells (repeat 0) 0 (repeat 0)

-- Parse the source string into Type Program
parse :: String -> Program
parse src = mapMaybe mapping src
    where
        mapping c = case c of
            '-' -> Just Decrement
            '+' -> Just Increment
            '<' -> Just MoveLeft
            '>' -> Just MoveRight
            '.' -> Just Output
            ',' -> Just Input
            '[' -> Just LoopL
            ']' -> Just LoopR
            x  -> Nothing

moveRight :: Cells a -> Cells a
moveRight (Cells xs y (z:zs)) = Cells (y:xs) z zs 

moveLeft :: Cells a -> Cells a
moveLeft (Cells (x:xs) y zs) = Cells xs x (y:zs)

--compile :: Cells Int -> Cells Token -> IO ()
--Define the compile functions for each Token here

{-runCompiler :: Program -> IO ()-}
{-runCompiler = compile initCells . prepareCells-}
    {-where-}
        {-prepareCells (x:xs) = Cells [] x xs-}

main = do
    programSrc <- getLine
    print $ parse programSrc

