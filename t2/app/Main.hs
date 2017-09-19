module Main where

import Lib
import System.Random
-- соритровка вставками. sort-by-insertions
--void sort (int* a) {
--    for(int i = 6; i > 0; --i) {
--        for(int j = 1; j < i;++j) {
--            if( (a[j-1]) > a[j]) {
--                swap(a[j-1], a[j]);
--            }
--        }
--    }
--}


sort'' :: (Ord a) => [a] -> [a]
sort'' [] = []
sort'' (x:xs) = insert x (sort'' xs)

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x > y     = y : insert x ys
  | otherwise = x : y : ys
  
data Pair = Pair String String
data DataBase = DataBase { key   :: String
                         , value :: String
                         }

dataBase :: [DataBase]
dataBase = [(DataBase "вода"      "wasser")
           ,(DataBase "молоко"    "milch")]




main :: IO ()
main = putStrLn . show $ sort'' [1,3,5,4,2]

