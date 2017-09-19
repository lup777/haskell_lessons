module Main where

import Lib
import Data.Typeable

lucki :: Int -> String
lucki 7 = "!Lucki number! :)"
lucki x = "No, sorry"

factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: (Show a) => [a] -> String
tell [x] = "Only 1 elem"
tell [x,y] = "Only 2 elems"
tell (x:y:z) = "many elems" ++ show x ++ show y ++ show z
tell [] = "empty list"

characters :: [Char] -> String
characters [x] = "String is to short"
characters [x,y] = "String is to short"
characters all@(x:y:xs) = show x ++ " is first and " ++ show y ++ " is the second characters in " ++ all
characters [] = "empty string"

bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "hudoi"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "tolstyi"
  | otherwise = "wirnyi"

head' :: [a] -> a
head' xs = case xs of
  [] -> error "empty list"
  (x:_) -> x


main :: IO ()
--main = do
--  str <- getLine
--  print (characters str)
--main = putStrLn (bmiTell 49.0)

--main = putStrLn . show $ [ (x, y, z) | x <- [1..10], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2, x + y + z == 24 ]

--main = putStrLn . show . head' $ ["aaa"]
max' :: Ord(a) => [a] -> a
max' [] = error "max' in empty list"
max' [x] = x
max' (x:xs) = max x (max' xs)

--main = putStrLn . show . max' $ [4,8,7,2,5,1]


replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x = [x] ++ (replicate (n - 1) x)

--main = do
--  putStrLn "enter list length: "
--  num <- readLn
--  putStrLn . show $ (replicate' num 5)


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' num _
  | num <= 0 = []
take' _ [] = []
take' 1 [x] = [x]
take' num (x:xs) = [x] ++ (take' (num - 1) xs)

--main = do
--  putStrLn "enter list length: "
--  num <- readLn
--  putStrLn . show $ (take' num [1,5,3,7,3,7,3])

reverce' :: [a] -> [a]
reverce' [] = []
reverce' (x:xs) = reverce' xs ++ [x]

--main = putStrLn . show $ reverce' [1,7,3,5,8]

repeate' :: a -> [a]
repeate' x = x:repeate' x

--main = putStrLn . show $ (take' 5 (repeate' 3))

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
--zip' (x:xs) (y:ys) = [(x,y)] ++ (zip' xs ys)
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

--main = putStrLn . show $ (zip' [1,2,3,4] ["a","b","c", "d", "e"])

--check that element is in the list
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
  | (n == x) = True
  | otherwise = n `elem'` xs

--main = putStrLn . show $ (elem' 3 [1,8,9,0,4,7,6,2,1,7,23,8,9,0])

sort' :: Ord a => [a] -> [a]
sort' [] = []
sort' (x:xs) =
  let
    small = sort' [a | a <- xs, a <  x]
    big   = sort' [a | a <- xs, a >= x]
  in
    small ++ [x] ++ big

--main = putStrLn . show $ (sort' [3,9,4,1,6,3,9])

max4' :: (Num a, Ord a) => a -> a
max4' = max 4

--main = putStrLn . show . max4' $ 5

inc' :: (Num a) => a -> a
inc' = (+ 1)

--main = putStrLn . show . inc' $ 4

apply_twise :: t -> (t -> t) -> t
apply_twise x f = f ( f x)

--main = putStrLn . show $ (apply_twise 1 (+ 2))
--main = putStrLn . show $ (apply_twise  "Hello World" (++ "!"))

zipWith' :: (t -> t -> t) -> [t] -> [t] -> [t]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = [f x y] ++ (zipWith' f xs ys)

--main = putStrLn . show $ (zipWith (+) [1,2,3] [3,2,1])

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

--main = putStrLn . show $ (flip' (>) 3 5)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
  | f x == True = x : filter' f xs
  | otherwise = filter' f xs

--main = putStrLn . show $ (filter' (> 3) [1,6,8,4,0,4,2,7,4,3,1,4,6])

mod' :: (Num a, Ord a) => a -> a -> a
mod' x y
  | x < y = x
  | otherwise = mod' (x - y) y


max'' (x:y:[])
  | x > y = x
  | otherwise = y
  
max'' (x:y:z)
  | x > y = max' (x : z)
  | otherwise = max' (y : z)


--main = putStrLn . show . max'' $ [x | x <- [1..100000], (mod' x 3829) == 0]

--main = putStrLn . show . head $ (filter p [100000,99999..])
--  where
--    p x = (mod' x 3829) == 0

--main = putStrLn . show . sum $ takeWhile (< 10000) (filter p [1..])
--  where
--    p x = odd (x^2)

--main = putStrLn . show . sum $ takeWhile (< 10000) (filter odd (map (^2) [1..]))
--  where
--    p x = odd


--main = putStrLn . show $ ( (\a b -> a + b) 3 4)

main = putStrLn . show $ (map (\a -> a + 1) [1..10])
