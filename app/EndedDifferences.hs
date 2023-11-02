module EndedDifferences (fromDifferences, getDifferences, calcDifference, calcDerivedDifference) where

import Table
import Dots
import Data.List (transpose)


fromDifferences :: [Char] -> [Dot] -> [[Double]] -> Table
fromDifferences sign dots differences = Table getNames
        $ transpose $ take 2 initedCells ++ map withDiffs (zip3 [0..] differences (drop 2 initedCells))
    where
        n = length dots - 1

        initedCells = transpose $ initCells dots
        initCells [Dot (x, y)] = [Cell (Just x) : Cell (Just y) : replicate n (Cell Nothing)]
        initCells ((Dot (x, y)):otherDots) =
            (Cell (Just x) : Cell (Just y) : replicate n (Cell Nothing)) :
            replicate (n + 2) (Cell Nothing) :
            initCells otherDots
        initCells [] = []

        getNames = Name "x" : Name "y" : differencesNames
        differencesNames =  map (\x -> Name (sign ++ show x ++ "y")) [1..n]

        withDiffs :: (Int, [Double], [Cell]) -> [Cell]
        withDiffs (_, [], cells) = cells
        withDiffs (currLevel, diff:otherDiffs, cells) = withDiffs (currLevel, otherDiffs, changeNth offset cells (Cell (Just diff)))
            where
                changeNth idx list newVal = take idx list ++ [newVal] ++ drop (idx+1) list
                offset = currLevel + indexInLevel * 2 + 1
                indexInLevel = n - currLevel - length otherDiffs - 1




getDifferences :: [Dot] -> ([Dot] -> Int -> Int -> Double) -> [[Double]] 
getDifferences dots func = calc 0 
    where 
        n = length dots - 1
        
        calc :: Int -> [[Double]]
        calc level = if level /= n
            then map (func dots level) [1..(n-level)] : calc (level + 1)
            else []


calcDifference :: [Dot] -> Int -> Int -> Double
calcDifference dots 0 idx = let 
    Dot (_, y_0) = dots!!(idx-1) 
    Dot (_, y_1) = dots!!idx 
        in y_1 - y_0
calcDifference dots level idx = calcDifference dots (level-1) (idx+1) - calcDifference dots (level-1) idx

calcDerivedDifference :: [Dot] -> Int -> Int -> Double 
calcDerivedDifference dots 0 idx = let 
    Dot (x_0, y_0) = dots!!(idx-1)
    Dot (x_1, y_1) = dots!!idx 
        in (y_1 - y_0) / (x_1 - x_0)
calcDerivedDifference dots level idx = 
            (calcDerivedDifference dots (level-1) (idx+1) - calcDerivedDifference dots (level-1) idx) / 
                            ( getSecond level idx - getFirst level idx )
    where 
        getFirst 0 idx' = let Dot (x_0, _) = dots!!(idx'-1) in x_0
        getFirst level' idx' = getFirst (level'-1) idx'
        
        getSecond 0 idx' = let Dot (x_1, _) = dots!!idx' in x_1
        getSecond level' idx' = getSecond (level'-1) (idx'+1)
            


