{-# LANGUAGE FlexibleInstances #-}
import Control.Monad.State
import Data.Functor
import Data.IORef
import Data.Array.IO
import System.IO
import System.Process
import System.Posix.Files
import Control.Monad
import System.FilePath.Posix
import System.Directory
import qualified Codec.Binary.UTF8.String as UTF8

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

data Transport = TCP | UDP | SCTP

checkProtocol :: Transport -> String
checkProtocol transport = case transport of
  TCP -> "That's TCP protocol."
  UDP -> "That's UDP protocol."
  SCTP -> "That's SCTP protocol."

main :: IO ()
main = putStrLn . checkProtocol $ TCP
-- let protocol = TCP

data Day = Sunday
         | Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         
data WorkMode = FiveDays | SixDays

workingDays :: WorkMode -> [Day]
workingDays FiveDays = [ Monday
                       , Tuesday
                       , Wednesday
                       , Thursday
                       , Friday
                       ]
workingDays SixDays = [ Monday
                      , Tuesday
                      , Wednesday
                      , Thursday
                      , Friday
                      , Saturday
                      ]

data IP = String

show_ip :: IO ()
show_ip = 
  let ip = "127.0.0.1"
  in
    putStrLn ip

apply2 :: (t -> t) -> t -> t
apply2 f x = f (f x)
-- apply2 4 -> 8

add_one :: [Int] -> [Int]
add_one [] = []
add_one (x:xy) = (x + 1) : add_one xy

-- ==== MAP pattern ====
--map' :: [t] -> (t -> t) -> [t]
--map' :: [Int] -> (Int -> Int) -> [Int]
map' [] f = []
map' (x:xy) f = f x : map' xy f



-- use it by: map' [1,2,3] (\x -> x * 3)

-- ==== FILTER pattern ====
filter' :: [Int] -> (Int -> Bool) -> [Int]
filter' [] f = []
filter' (x:xs) f
  | (f x == True) = x : filter' xs f
  | otherwise     = filter' xs f
-- use example:
-- filter' [1,2,3,4,5,6] (\x -> x 'mod' 2 == 0)

-- ==== PARTIAL FUNCTIONS ====
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x


data FailableDouble = Failure
                    | Ok Double
  deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = Ok (x / y)

data Person = Person String Int String
  deriving Show

showPersonAge :: Person -> String
showPersonAge p@(Person _ x _) =
  "Age of " ++ show p ++ " is " ++ show x

useSafeDiv :: Double -> Double -> Double
useSafeDiv x y = case (safeDiv x y) of
  Ok result -> result
  otherwise -> 0

data IntList = Empty | Cons Int IntList
  deriving Show

var :: IntList -- список
var = Cons 1 (Cons 2 (Cons 3 Empty))

incList :: IntList -> IntList
incList Empty = Empty
incList (Cons x xs) = Cons (x + 1) (incList xs)

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

data Foo = F Int | G Char
instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G g1) == (G g2) = g1 == g2
  _ == _ = False
  foo1 /= foo2 = not (foo1 == foo2)

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)


class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  --toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree' a = Empty' | Node' a (Tree' a) (Tree' a)

instance Listable (Tree' Int) where
  toList Empty' = []
  toList (Node' x l r) = toList l ++ [x] ++ toList r

myTree :: Tree' Int
myTree = Node'  1 (Node' 2 Empty' (Node' 3 Empty' Empty')) (Node' 4 Empty' Empty')


-- MONADS --

askName :: IO ()
askName = do
  putStr "Enter Your name\n"         -- String -> IO ()
  name <- getLine                    --           IO String
  putStr $ "Hello " ++ name ++ "\n"  -- String -> IO ()

-- return - если надо произвести в монаде некие вычисление
-- не связанные смонадой
-- return :: a -> m a

doubleText = do
  putStr "Please, enter text: "  -- String -> IO ()
  text <- getLine                --           IO String
  dt <- return $ text ++ text    --           IO String
  putStr $ dt ++ "\n"            -- String -> IO ()

doubleText' = do
  putStr "Please, enter text: "  -- String -> IO ()
  text <- getLine                --           IO String
  let dt = text ++ text          
  putStr $ dt ++ "\n"            -- String -> IO ()

-- readFile :: FilePath -> IO String

printFileContent :: IO ()
printFileContent = do
  fileContent <- readFile "haskell_lessons/algo.hs"
  putStr fileContent


-- runState :: State s a -> (a, s)
-- modify   :: MonadState s m => (s -> s) -> m ()
stateMonadExample = do
  -- runState (do modify (+(1))) :: Num s => s -> ((), s)
  let r = runState (do
                       modify (+1) -- (Num s, MonadState s m) => m ()
                       modify (*2)
                       modify (+3)
                   ) 5
  print r
      


-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- get :: MonadState s m => m s
-- "<-" стрелка снимает монадический тип
printFromStateMonad = do
  r <- runStateT (do
                     modify (+1)
                     modify (*2)
                     s <- get
                     lift $ print s -- State Int IO ()
                     modify (+3)
                 ) 5
  print r


-- как заменить Double
wodo = (putStr "Как Вас зовут?\n") >> (putStr "Сколько Вам лет?\n")

-- readLn :: Read a => IO a
-- getLine :: IO String

ask = do putStr "Как Вас зовут? "
         a <- getLine
         print $ "Hi, " ++ a

-- and the same ..
ask' = getLine >>= print
ask'' = getLine >>= (\a -> print a)

ask''' = putStr "Как Вас зовут? " >>
         getLine >>= \i ->
                       putStr "Сколько Вам лет? " >>
                       getLine >>= \v ->
                                     putStr "Привет, " >>
                                     putStr i >>
                                     putStr "! Неплохой денёк сегодня!\n"


a1 = do x <- [10, 100, 1000]
        y <- [1, 2, 3]
        return (x*y)

--  askName = do
--  putStr "Enter Your name\n"         -- String -> IO ()
--  name <- getLine                    --           IO String
--  putStr $ "Hello " ++ name ++ "\n"  -- String -> IO ()

askName' = putStr "Как Вас зовут? " >>
           getLine >>= \v ->
                         putStr $ "Hello " ++ v ++ "\n"

-- Class Monad {
--   >>=     // связывание. комбинирует два монадических значения
--   >>      // то же, но когда функция не нуждается в значении
--           // произведённом первым монадичским оператором
--           // как именно комбинируются значения зависит от реализации монад
--   return  // вставляет занчение в монаду
--
--   do e1 ; e2       = e1 >> e2
--   do p <- e1; e2   = e1 >>= \p -> e2
-- }

-- class  Monad m  where
--     (>>=)        :: m a -> (a -> m b) -> m b
--     (>>)         :: m a -> m b -> m b
--     return       :: a -> m a
--     fail         :: String -> m a
-- 
--     m >> k       = m >>= \_ -> k

-- class (Monad m) => MonadPlus m where
--     mzero           :: m a
--     mplus           :: m a -> m a -> m a

-- m >>= \x -> mzero  =  mzero
-- mzero >>= m        =  mzero
-- m `mplus` mzero  =  m
-- mzero `mplus` m  =  m
                               
askKey = do 'a' <- getChar
            print "Hello!"


-- return a >>= k            = k a
-- x >>= return              = m
-- xs >>= return . f         = fmap f xs
-- m >>= (\x -> k x >>= h)   = (m >>= k) >>= h


class Functor' f where
  fmap' :: (a -> b) -> (f a -> f b)


data Tree_ a = Leaf_ a | Branch_ (Tree_ a) a (Tree_ a)
  deriving Show

instance Functor' Tree_ where
  fmap' f (Leaf_ x) = Leaf_ (f x)
  fmap' f (Branch_ l x r) = Branch_ (fmap' f l) (f x) (fmap' f r)


showTree = fmap' (+1) (Branch_ (Leaf_ 2) 3 (Leaf_ 4))

--- USING VARIABLES ---
testVar = do varA <- newIORef 0 -- create and init a new variable
             a0 <- readIORef varA
             writeIORef varA 1
             a1 <- readIORef varA
             print (a0, a1)

twoKeys = do let get2chars = getChar >> getChar
             putStr "Press two keys"
             get2chars
             return () -- чтобы ничего не выводилось в конце

ioActions :: [IO ()]
ioActions = [(print "Hello!"),
             (putStr "just kidding"),
             (getChar >> return ())]

callIoAction = do head ioActions
                  ioActions !! 1
                  last ioActions

callIoAction' = (head ioActions) >> (ioActions !! 1) >> (last ioActions)

boolIoAction :: IO Bool
boolIoAction = putStr "Нажми y " >> 
               getLine >>= \r -> return (r == "y")


while :: IO Bool -> IO ()
while action = do x <- action
                  case x of
                    True  -> print "GoodBy!"
                    False -> while action

readi :: Handle -> Integer -> IO Char
readi h i = do hSeek h AbsoluteSeek i
               hGetChar h

readfileChar :: String -> IO (Integer -> IO Char)
readfileChar name = do h <- openFile name ReadMode
                       return (readi h)

readTest :: String -> Integer -> IO Char
readTest name num = (readfileChar name) >>= \r -> r num

readFileContent :: FilePath -> IO String
readFileContent name = do readFile name
                          
callCmd :: String -> IO String
callCmd cmd = do (_, Just hout, _, _) <- createProcess (proc cmd []){ std_out = CreatePipe }
                 hGetContents hout

callCmd' :: String -> [String] -> IO String
callCmd' cmd opts = do (_, Just hout, _, _) <- createProcess (proc cmd opts){ std_out = CreatePipe }
                       hGetContents hout

splitLines :: IO String -> IO [String]
splitLines str = do x <- str
                    return (lines x)


test' :: IO ()
test' = do  x <- listFolder "./"
            status <- getFileStatus (head x)
            if (isDirectory status)
              then print ("[" ++ (head x) ++ "]")
              else print ((head x) ++ "")
            return ()

test'' :: IO [String] -> IO ()

test'' io_arr = do (x:xs) <- io_arr
                   print x
                   test'' (return (xs))

apply :: (a -> b) -> [a] -> [b]
apply f [] = []
apply f (x:xs) = f x : apply f xs

test''' :: IO [String] -> IO ()
test''' io_arr = do x <- io_arr
                    io_apply putStrLn x
                    return ()

listFolder :: FilePath -> IO [String]
listFolder path = do x <- (callCmd' "ls" [path])
                     return (lines x)


ioIsDirectory :: IO FileStatus -> IO Bool
ioIsDirectory io_status = do status <- io_status
                             return (isDirectory status)

--ioArrToArrOfIO :: IO [a] -> [IO a]
--ioArrToArrOfIO io_arr = do x <- io_arr
--                           let \f a ->


io_apply :: (a -> IO ()) -> [a] -> IO ()
io_apply f [] = do return ()
io_apply f (x:xs) = do f x
                       io_apply f xs
                       return ()

tt :: Num a => [a] -> [a]
tt arr = let f = \xs -> case xs of
                          (x:xs) -> x + 1 : f xs
                          []     -> []
         in f arr


headM :: IO [String] -> IO String
headM io_arr = do arr <- io_arr
                  return (head arr)

                      
-- listFolder    :: FilePath -> IO [String]
-- getFileStatus :: FilePath -> IO FileStatus
-- isDirectory   :: FileStatus -> Bool
-- ioIsDirectory :: IO FileStatus -> IO Bool

isDir :: String -> IO Bool
isDir str = do status <- getFileStatus str
               print str
               return $ isDirectory $ status

isNotDir :: String -> IO Bool
isNotDir str = do status <- getFileStatus str
                  return $ not $ isDirectory $ status


grepFolders :: IO [String] -> IO [String]
grepFolders io_arr = do arr <- io_arr
                        --filterM isDir arr
                        filterM doesDirectoryExist arr

grepFolders' :: FilePath -> IO [FilePath]
grepFolders' path = do arr <- getDirectoryContents path
                        --filterM isDir arr
                       filterM doesDirectoryExist arr

grepFolders'' :: FilePath -> IO ()
grepFolders'' path = do arr <- getDirectoryContents path
                        let pif = \e_ -> do e <- doesDirectoryExist e_
                                            case e of
                                              True  -> print e_
                                              False -> return ()
                        mapM_ pif arr

notM :: IO Bool -> IO Bool
notM io_b = do b <- io_b
               return $ not b

grepFiles :: IO [String] -> IO [String]
grepFiles io_arr = do arr <- io_arr
                      filterM (\d -> notM $ doesDirectoryExist d) arr

--applyM :: (a -> IO b) -> IO [a] -> IO [b]
--applyM f io_list = 

showPathList :: IO [String] -> IO ()
showPathList io_list = do let show_ = \e -> putStrLn e
                          list <- io_list
                          io_apply show_ list
                          return ()

t' :: IO [String]
t' = grepFolders (listFolder "/home/alexander/")

t'' :: IO [String]
t'' = grepFiles (listFolder "/home/alexander/")
--t' :: IO ()
--t' = showPathList $ grepFolders $ listFolder $ makeValid "/home/alexander/"

t''' :: FilePath -> IO ()
t''' path = getDirectoryContents path >>=
            filterM doesFileExist >>=
            mapM_ print

getQualifiedDirectoryContents :: FilePath -> IO [FilePath]
getQualifiedDirectoryContents fp =
    map (fp </>) . filter (`notElem` [".",".."]) <$> getDirectoryContents fp

--                           (<$>) :: Functor f => (a -> b) -> f a -> f b
-- ( <$> getDirectoryContents "/") :: ([FilePath] -> b) -> IO b
-- ( <$> getDirectoryContents)     :: (IO [FilePath] -> b) -> FilePath -> b

folders :: FilePath -> IO ()
folders p = print
            =<< filterM doesDirectoryExist
            =<< getQualifiedDirectoryContents p

--files :: FilePath -> IO ()
--files p = print $ filter not <$> doesDirectoryExist $ getQualifiedDirectoryContents p
        
