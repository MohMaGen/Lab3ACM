{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Dots
import EndedDifferences
import Leibnitz (createLeibnitzPolinom)
import Formatting
import Table
import Newton (createNewtonPolinom)


outputDots:: (String -> IO()) -> [Dot] -> IO()
outputDots output [] = output ""
outputDots output ((Dot (x, y)):other) = do
    output $ formatToString ((left 6 ' ' %. fixed 3) % "\t" % (left 6 ' ' %. fixed 3) % "\n") x y;
    outputDots output other


offset :: String
offset = "\t"


errors :: (Double -> Double) -> [Dot] -> [Double]
errors polinom dots = [y_i - polinom x_i | Dot (x_i, y_i) <- dots]

printErrors :: (Double -> Double) -> [Dot] -> Int -> IO ()
printErrors polinom dots n = do
    let err = sqrt . (/ fromIntegral (length dots + 1)) . sum . map (\x -> x * x) $ errors polinom dots;
    let errExepted = sqrt . (/ fromIntegral (length dots - n)) . sum . map (\x -> x * x) $ errors polinom dots;
    print . Table [Name "errors", Name "squared"] . map (\ x -> [Cell (Just x), Cell (Just $ x * x)]) $ errors polinom dots;

    print $ Table [Name "All", Name "Not used"] [[Cell (Just err), Cell (Just errExepted)]]

main :: IO ()
main = do
    input <- readFile "variant90.txt";
    let dots = map ((\ [x, y] -> Dot (read x, read y)) . words) (lines input);
    let diffs = getDifferences dots calcDifference;
    let derDiffs = getDifferences dots calcDerivedDifference;

    let diffsSize = map (\ col -> Cell (Just (maximum col - minimum col))) diffs;


    putStr . show $ fromDifferences "Δ" dots diffs;


    putStr $ replicate (cellLen + 1) ' ' ++ topLine (length diffsSize + 1);
    putStr $ replicate (cellLen + 1) ' ' ++ show (Name "d_m") ++ showCells diffsSize;
    putStrLn $ replicate (cellLen + 1) ' ' ++ bottomLine (length diffsSize + 1);

    print $ fromDifferences "δ" dots derDiffs;

    writeFile "logrange.txt" "";
    let logrange = createLeibnitzPolinom [head dots, dots!!5, dots!!10];
    outputDots (appendFile "logrange.txt") [ Dot (x, logrange x) | Dot(x, _) <- dots]

    writeFile "newton.txt" "";
    let newtonIndecies = [1, 3, 5, 11] :: [Int];
    let newtonDots = map snd . filter ((`elem` newtonIndecies) . fst) $ zip [1..] dots;
    let newtonDiffs = getDifferences newtonDots calcDerivedDifference;
    let newton = createNewtonPolinom newtonDots newtonDiffs;
    outputDots (appendFile "newton.txt") [ Dot (x, newton x) | Dot(x,_) <- dots]

    print $ fromDifferences "δ" newtonDots newtonDiffs;

    printErrors logrange dots 2;
    printErrors newton dots 3;



