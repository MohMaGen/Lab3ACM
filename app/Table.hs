module Table (Table(Table), Cell(Cell), Name(Name)) where

import Formatting

data Table  = Table [Name] [[Cell]]

newtype Cell = Cell (Maybe Double)
newtype Name = Name String

cellLen :: Int
cellLen = 8

instance Show Table where
    show (Table _ []) = ""
    show (Table names (first:otherLines)) =  firstLine ++ showNames names ++ nameCellsLine ++ showCells (first:otherLines) ++ lastLine
        where 
            firstLine = "┌" ++ concat ( replicate (length first - 1) (replicate cellLen '─' ++ "┬"))  
                ++ (replicate cellLen '─' ++ "┐") ++ "\n"

            nameCellsLine = "├" ++ concat ( replicate (length first - 1) (replicate cellLen '─' ++ "┼"))  
                ++ (replicate cellLen '─' ++ "┤") ++ "\n"

            lastLine = "└" ++ concat ( replicate (length first - 1) (replicate cellLen '─' ++ "┴"))  
                ++ (replicate cellLen '─' ++ "┘") ++ "\n"

            showCells [] = ""
            showCells ([]:otherLines') = "│\n" ++ showCells otherLines'
            showCells ((cell:otherCells):otherLines') = show cell ++ showCells (otherCells:otherLines')

            showNames (name:otherNames) = show name ++ showNames otherNames 
            showNames [] = "│\n"


instance Show Cell where
    show (Cell (Just value)) = cellFormat value
        where 
            cellFormat = formatToString $ "│" % (center cellLen ' ' %. fixed 2)
    show (Cell Nothing) = "│" ++ replicate cellLen ' '

instance Show Name where 
    show (Name name) = nameFormat name
        where 
            nameFormat = formatToString $ "│" % center cellLen ' '





