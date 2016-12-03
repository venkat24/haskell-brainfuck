module Main where

import Data.Maybe

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

type Program = [Token]

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

main = do
    programSrc <- getLine
    print $ parse programSrc

