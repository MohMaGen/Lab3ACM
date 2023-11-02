module Leibnitz (createLeibnitzPolinom) where

import Dots

createLeibnitzPolinom :: [Dot] -> (Double -> Double)
createLeibnitzPolinom dots = \x -> 
            sum  [y_i *  production x x_i i| (i, x_i, y_i) <- dots_iter]
        where
            xs_without :: Int -> [Double]
            xs_without i = [x | (j, x, _) <- dots_iter, j /= i] 
            
            dots_iter = zip3 [0..] xs ys
            xs = [x | Dot(x, _) <- dots]
            ys = [y | Dot(_, y) <- dots]


            production x x_i i = product [(x - x_j) / (x_i - x_j) | x_j <- xs_without i]

