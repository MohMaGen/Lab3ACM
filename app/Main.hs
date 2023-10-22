module Main where

import Dots
import Table


dots :: [Dot]
dots = [Dot (0, 0), Dot (1, 2), Dot (2, 3), Dot (3, 4), Dot (5, 4)]

table = Table [Name "biba", Name "boba", Name "frog", Name "toad"] [
    [Cell (Just 10), Cell Nothing, Cell (Just 10), Cell (Just 35)],
    [Cell Nothing, Cell (Just 20.21), Cell Nothing, Cell (Just 20)],
    [Cell (Just 10), Cell Nothing, Cell (Just 10.32), Cell (Just 35)],
    [Cell (Just 10), Cell Nothing, Cell (Just 10), Cell (Just 35)],
    [Cell Nothing, Cell (Just 20), Cell Nothing, Cell (Just 10.123)],
    [Cell Nothing, Cell (Just 2), Cell Nothing, Cell (Just 40)]]





main :: IO ()
main = do 
    print table; padding;

    where 
        padding = do putStrLn ""; putStrLn "";

