
test = putStrLn "hi"

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) | x <= y = x : y : ys
                  | True = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- isort [5,4,3,2,1]
-- insert 5 (isort [4,3,2,1])
-- insert 5 (insert 4 (isort [3,2,1]))
-- insert 5 (insert 4 (insert 3 (isort [2,1])))
-- insert 5 (insert 4 (insert 3 (insert 2 (isort [1]))))
-- insert 5 (insert 4 (insert 3 (insert 2 (insert 1 (isort [])))))

-- insert 5 (insert 4 (insert 3 (insert 2 (insert 1 []))))
-- insert 5 (insert 4 (insert 3 (insert 2 [1] )))
-- insert 5 (insert 4 (insert 3 ( 1 : insert 2 [] )))
-- insert 5 (insert 4 (insert 3 ( 1 : [2] )))
-- insert 5 (insert 4 ( 1 : insert 3 [2] ))
-- insert 5 (insert 4 ( 1 : 2 : insert 3 [] ))
-- insert 5 (insert 4 ( 1 : 2 : [3] ))
-- insert 5 ( 1 : insert 4 (2 : [3]) )
-- insert 5 ( 1 : 2 : insert 4 [3] )
-- insert 5 ( 1 : 2 : 3 : insert 4 [] )
-- insert 5 ( 1 : 2 : 3 : [4] )
-- 1 : insert 5 2 : 3 : [4]
-- 1 : 2 : insert 5 3 : [4]
-- 1 : 2 : 3 : insert 5 [4]
-- 1 : 2 : 3 : 4 : insert 5 []
-- 1 : 2 : 3 : 4 : [5]
-- [1,2,3,4,5]


sort' :: (Ord a) => [a] -> [a]
sort' []     = []
sort' (x:xs) = insert' x (sort' xs)
  
insert' :: (Ord a) => a -> [a] -> [a]
insert' x []     = [x]
insert' x (y : xs) | x > y = y : insert' x xs
                   | True  = x : y : xs
