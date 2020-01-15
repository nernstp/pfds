
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(x:xs) = l:suffixes xs