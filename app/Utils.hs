module Utils ((!?)) where 

(!?) :: [a] -> Int -> Maybe a
list !? idx = if idx < 0 || idx >= length list then Nothing else Just $ list!!idx
