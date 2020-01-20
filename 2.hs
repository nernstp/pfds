data BTree a = Empty | Branch (BTree a) a (BTree a)
    deriving Show 
    
t = Branch (Branch Empty 3 Empty) 5 Empty
t2 = Branch Empty 5 (Branch Empty 6 Empty)

member Empty x = False
member (Branch l a r) x = if a==x then True else if x<a then member l x else member r x

insert Empty x = Branch Empty x Empty
insert n@(Branch l a r) x = if a==x then n else if x<a then Branch (insert l x) a r else Branch l a (insert r x)


--2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes l@(x:xs) = l:suffixes xs

--2.2
member2 Empty x = False
member2 n@(Branch l a r) x = recu a n
                            where recu y Empty = x==y
                                  recu y (Branch l v r) = if x < v then recu y l else recu v r 

--2.3
insert2 Empty x = Branch Empty x Empty
--insert (Branch l a r) x low high | x<a = Branch (insert l x) a r
--                                 | x>a = Branch l a (insert r x)
--                                 | otherwise = (Branch l a r)
