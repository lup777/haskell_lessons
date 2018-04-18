import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Writer
import Control.Monad.Reader
import Data.Word

data Game = Game
  { _score :: Int
  , _units :: [Unit]
  , _boss  :: Unit
  } deriving (Show)

data Unit = Unit
  { _health   :: Int
  , _position :: Point
  } deriving (Show)

data Point = Point
  { _x :: Double
  , _y :: Double
  } deriving (Show)

score :: Lens' Game Int
score = lens _score (\game v -> game {_score = v})

boss :: Lens' Game Unit
boss = lens _boss (\game v -> game { _boss = v })

health :: Lens' Unit Int
health = lens _health (\unit v -> unit { _health = v })

position :: Lens' Unit Point
position = lens _position (\unit v -> unit { _position = v })

x :: Lens' Point Double
x = lens _x (\point v -> point { _x = v })

y :: Lens' Point Double
y = lens _y (\point v -> point { _y = v })


initialState :: Game
initialState = Game
    { _score = 0
    , _units =
        [ Unit
            { _health = 10
            , _position = Point { _x = 3.5, _y = 7.0 }
            }
        , Unit
            { _health = 15
            , _position = Point { _x = 1.0, _y = 1.0 }
            }
        , Unit
            { _health = 8
            , _position = Point { _x = 0.0, _y = 2.1 }
            }
        ]
    , _boss = Unit
        { _health = 100
        , _position = Point { _x = 0.0, _y = 0.0 }
        }
    }

strike :: StateT Game IO ()
strike = do
    lift $ putStrLn "*shink*"
    boss.health -= 10


-- Это всё отсюда https://habrahabr.ru/post/190442/
-- но пока надо разобратся с тройкой монад
-- Writer
-- Reader
-- State

half :: Int -> Writer String Int
half x = do
  tell ("I just halved " ++ (show x) ++ "!")
  return (x `div` 2)

--t = half <=< half $ 8

--t' = runWriter $ half 8
--t'' = runWriter $ half 8 >>= half


greeter :: Reader String String
greeter = do
  name <- ask
  return ("hello, " ++ name ++ "!")

--t''' = runReader greeter $ "adit"


-- list monad

-- [3,4,5] >>= \x -> [x,-x]
-- [3,4,5] >>= \x -> ['a','b'] >>= \y -> return (x, y)
-- [3,4,5] >>= \x -> ['a','b'] >>= \y -> [(x, y)]

--t = do getLine >>= \s -> getLine >>= \x -> return (s ++ x)
-- [((+) 1), ((-)1)] >>= \x -> [x 3,x 3]
-- :result [4,4,-2,-2]

-- <*Main> [((+) 1), ((-)1)] >>= \x -> [x 3]
-- [4,-2]

guard' :: (MonadPlus m) => Bool -> m ()
guard' True = return ()
guard' False = mzero

filterSeven = [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
filterSeven' = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x

-- <*Main> Nothing >>= \x -> return (x + 1)
-- Nothing


data People = Boy {name :: String}
            | Girl {name :: String}
  deriving Show

instance Monoid People where
  mappend a b = Boy ((name a) ++ " + " ++ (name b))
  mempty = Boy ""

--t :: People
--t = v <> m
--  where
--    v = (Boy "Вася")
--    m = (Girl "Маша")


mt :: Integer -> Writer [Integer] Integer
mt n = do let x = n `div` 2
          tell [n `mod` 2]                   
          if x > 0
            then mt x
            else return x

t' :: Integer -> Writer [String] Integer
t' n = do let x = n `div` 2
          tell [(show n) ++ " mod 2 = " ++ (show (n `mod` 2))]
          if x > 0
            then t' x
            else return x

revert :: [a] -> [a]
revert []     = []
revert (x:xs) = (revert xs) ++ [x]

decToBinary x = revert . snd . runWriter $ (mt x)
            
-- http://fprog.ru/2009/issue1/dan-piponi-haskell-monoids-and-their-uses/


add :: Integer -> Integer -> Writer [String] Integer
add x y = do
  tell [(show x) ++ " + " ++ (show y) ++ " = " ++ (show $ x + y)]
  return $ x + y

inc :: Integer -> Writer [String] Integer
inc x = do
  tell ["increment" ++ (show x) ++ " -> " ++ (show $ x + 1)]
  return $ x + 1

--t :: Integer -> Integer -> Integer
--t x y = do runWriter $ add x (add 2 3)

-- add 2 3 >>= add 4
-- inc 2 >>= inc >>= inc >>= inc

--t = runWriter $ inc 2
--    >>= \x -> return $ runWriter $ inc x
       
t1 = runWriter $ inc 3


intToHexHelper''' :: Int -> Writer String String
intToHexHelper''' x = do tell (toText remainder)
                         if divresult >= 16
                           then intToHexHelper''' divresult
                           else tell (toText divresult) >> return ""
  where
    divresult = x `div` 16
    remainder = x - (divresult * 16)
    toText x = case x of
                 10 -> "A"
                 11 -> "B"
                 12 -> "C"
                 13 -> "D"
                 14 -> "E"
                 15 -> "F"
                 _  -> show x

intToHexStr''' x = revert . snd $ runWriter (intToHexHelper''' x)

intToHexStr' :: Int -> String
intToHexStr' x
  | x >= 16 = (intToHexStr' divresult) ++ (toText remainder)
  | True    = (toText x)
  where
    divresult = x `div` 16
    remainder = x - (divresult * 16)
    toText x = case x of
                 10 -> "A"
                 11 -> "B"
                 12 -> "C"
                 13 -> "D"
                 14 -> "E"
                 15 -> "F"
                 _  -> show x

intToHexStr'' :: Int -> Writer [String] String
intToHexStr'' x
  | x >= 16 = tell [(show x) ++ " / 16 -> " ++ toText remainder]
              >> intToHexStr'' divresult >>= \s -> return $ s ++ (toText remainder)
  | True    = tell [(show x) ++ " / 16 -> " ++ toText x]
              >> return (toText x)
              
  where
    divresult = x `div` 16
    remainder = x - (divresult * 16)
    toText x = case x of
                 10 -> "A"
                 11 -> "B"
                 12 -> "C"
                 13 -> "D"
                 14 -> "E"
                 15 -> "F"
                 _  -> show x

intToHexStr :: Int -> Int -> Writer [String] String
intToHexStr x n
  | x >= n = tell [logMsg remainder]
             >> intToHexStr divresult n >>= \s -> return $ s ++ (toText remainder)
  | True   = tell [logMsg x]
             >> return (toText x)
  where
    divresult = x `div` n
    remainder = x - (divresult * n)
    logMsg i = (show x) ++ " / " ++ (show n) ++ " -> " ++ toText i
    toText x = case x of
                 10 -> "A"
                 11 -> "B"
                 12 -> "C"
                 13 -> "D"
                 14 -> "E"
                 15 -> "F"
                 _  -> show x
