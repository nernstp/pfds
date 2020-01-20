
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(x:xs) = l:suffixes xs



data BTree a = Empty | Branch (BTree a) a (BTree a)
    deriving Show 

t = Branch (Branch Empty 3 Empty) 5 Empty
t2 = Branch Empty 5 (Branch Empty 6 Empty)
    
member Empty x = False
member n@(Branch l a r) x = recu a n
                            where recu y Empty = x==y
                                  recu y (Branch l v r) = if x < v then recu y l else recu v r 

insert Empty x = Branch Empty x Empty
--insert (Branch l a r) x low high | x<a = Branch (insert l x) a r
--                                 | x>a = Branch l a (insert r x)
--                                 | otherwise = (Branch l a r)