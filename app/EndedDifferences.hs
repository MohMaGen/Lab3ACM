module EndedDifferences (fromDots, calcDifferences) where

import Table
import Dots
import Data.List(transpose)



fromDots :: [Dot] -> ([[Cell]] -> [[Cell]]) -> Table
fromDots dots func = Table getNames (func $ initCells dots)
    where
        n = length dots - 1

        initCells [Dot (x, y)] = [Cell (Just x) : Cell (Just y) : replicate n (Cell Nothing)]
        initCells ((Dot (x, y)):otherDots) =
            (Cell (Just x) : Cell (Just y) : replicate n (Cell Nothing)) :
            replicate (n + 2) (Cell Nothing) :
            initCells otherDots
        initCells [] = []

        getNames = Name "x" : Name "y" : differencesNames
        differencesNames =  map (\x -> Name ("Δ" ++ show x ++ "y")) [1..n]

calcDifferences :: [[Cell]] -> [[Cell]]
calcDifferences cells = transpose $ map' calcs (transpose indexed) []
    where
        calc :: ((Int, Int), Cell) -> [[Cell]] -> Cell
        calc ((i, j), cell) cells' = if j < 2 then cell else
                case diffs of
                    (Just (Cell (Just first)), Just (Cell (Just second))) -> Cell (Just $ second - first)
                    _ -> Cell Nothing
            where
                list !? idx' = if idx' < 0 || idx' >= length list then Nothing else Just $ list !! idx'
                diffs = (cells'!?(i-1) >>= (!?(j-1)), cells'!?(i+1) >>= (!?(j-1)))
        
        calcs :: [((Int, Int), Cell)] -> [[Cell]] -> [Cell]
        calcs arr ch = map (`calc` ch) arr

        indexed :: [[((Int, Int), Cell)]]
        indexed = zipWith (\ idx cells' -> map (\ (jdx, cell) -> ((idx, jdx), cell)) cells') [0..] (map (zip [0..]) cells)

        map' :: (a-> [b] -> b) -> [a] -> [b] -> [b]
        map' func (curr:othr) changed = map' func othr $ changed ++ [func curr changed]
        map' _ [] changed = changed



