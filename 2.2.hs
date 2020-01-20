
data BTree a = Empty | Branch (BTree a) a (BTree a)
    deriving Show 

insert Empty x  _ _ = Branch Empty x Empty
insert (Branch l a r) x low high | x<a = Branch (insert l x) a r
                                 | x>a = Branch l a (insert r x)
                                 | otherwise = (Branch l a r)