module Newton (createNewtonPolinom) where

import Dots

createNewtonPolinom :: [Dot] -> [[Double]] -> (Double -> Double)
createNewtonPolinom dots diffs = \x -> y_0 + sum [delta j * product [x - x_i | x_i <- xs_before j] | j <- [1..m]]
  where
    delta j' = head $ diffs !! (j'-1)
    m = length dots - 1
    xs_before j' = take j' $ map getX dots
    y_0 = getY $ head dots 

