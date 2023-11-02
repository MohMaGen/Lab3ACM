module Table (
    Table(Table), 
    Cell(Cell), 
    Name(Name), 
    showCells, 
    topLine, 
    centerLine, 
    bottomLine,
    cellLen,
) where

import Formatting

data Table  = Table [Name] [[Cell]]

newtype Cell = Cell (Maybe Double)
newtype Name = Name String

cellLen :: Int
cellLen = 8


instance Show Table where
    show (Table _ []) = ""
    show (Table names (first:otherLines)) =  
                topLine (length first) ++ 
                showNames names ++ 
                centerLine (length first) ++ 
                showRow (first:otherLines) ++ 
                bottomLine (length first)
        where
            showRow [] = ""
            showRow (curr:otherLines') = showCells curr ++ showRow otherLines'

            showNames (name:otherNames) = show name ++ showNames otherNames
            showNames [] = "│\n"

topLine :: Int -> String
topLine n = "┌" ++ concat ( replicate (n-1) (replicate cellLen '─' ++ "┬"))
    ++ (replicate cellLen '─' ++ "┐") ++ "\n"

centerLine :: Int -> String
centerLine n = "├" ++ concat ( replicate (n-1) (replicate cellLen '─' ++ "┼"))
    ++ (replicate cellLen '─' ++ "┤") ++ "\n"

bottomLine :: Int -> String
bottomLine n = "└" ++ concat ( replicate (n-1) (replicate cellLen '─' ++ "┴"))
    ++ (replicate cellLen '─' ++ "┘") ++ "\n"

instance Show Cell where
    show (Cell (Just value)) = cellFormat value
        where
            cellFormat = formatToString $ "│" % (center cellLen ' ' %. fixed 3)
    show (Cell Nothing) = "│" ++ replicate cellLen ' '

showCells :: [Cell] -> String
showCells = foldr ((++) . show) "│\n"


instance Show Name where
    show (Name name) = nameFormat name
        where
            nameFormat = formatToString $ "│" % center cellLen ' '





