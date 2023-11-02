module Dots (Dot (Dot), getX, getY, toPair) where

newtype Dot = Dot (Double, Double)

getX :: Dot -> Double 
getX (Dot(x,_)) = x

getY :: Dot -> Double 
getY (Dot(_,y)) = y

toPair :: Dot -> (Double, Double)
toPair (Dot v) = v

instance Show Dot where 
    show (Dot(x, y)) = "( " ++ show x ++ " " ++ show y ++ " )"

