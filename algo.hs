{-# LANGUAGE FlexibleInstances #-}
import Control.Monad.State
import Control.Conditional (ifM)
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
import Text.Read
import Text.Show.Functions

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
                     s <- Control.Monad.State.get
                     Control.Monad.State.lift $ print s -- State Int IO ()
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

--tt :: Num a => [a] -> [a]
--tt arr = let f = \xs -> case xs of
--                          (x:xs) -> x + 1 : f xs
--                          []     -> []
--         in f arr


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
--    map (fp </>) . filter (`notElem` [".",".."]) <$> getDirectoryContents fp
  map (fp </>) . filter isExpectedFile' <$> getDirectoryContents fp
  

--                           (<$>) :: Functor f => (a -> b) -> f a -> f b
-- ( <$> getDirectoryContents "/") :: ([FilePath] -> b) -> IO b
-- ( <$> getDirectoryContents)     :: (IO [FilePath] -> b) -> FilePath -> b

isExpectedFile :: String -> Bool
isExpectedFile s = case head s of
                     '.' -> False
                     _   -> True

isExpectedFile' ('.':[]) =  True
isExpectedFile' ('.':'.':_) = True
isExpectedFile' ('.':_) = False
isExpectedFile' _ = True

folders :: FilePath -> IO [FilePath]
folders p = filterM doesDirectoryExist
            =<< getQualifiedDirectoryContents p

files :: FilePath -> IO [FilePath]
files p = filterM doesFileExist
          =<< getQualifiedDirectoryContents p

printList :: IO [String] -> IO ()
printList iol = do l <- iol
                   times <- mapM getModTime l
                   let t_n = merge [0..(length l)] times
                   let result = merge t_n l
                   mapM print result
                   return ()
-- getModTime ""
--timeModifcationArray :: IO [String] -> IO []

merge :: (Show b) => [b] -> [String] -> [String]
merge []     _      = []
merge _      []     = []
merge (x:xs) (y:ys) = (show x ++ " " ++ y) : (merge xs ys)

--listToNumberingList :: (Show a) => [a] -> [String]
--listToNumberingList = 


ioLength :: IO [a] -> IO Int
ioLength x = fmap (length) x

ioOrderingNumbers :: IO [a] -> IO [Int]
ioOrderingNumbers iol = do len <- ioLength iol
                           return [1..len]

allEntries :: FilePath -> IO [FilePath]
allEntries p = do l1 <- files p
                  l2 <- folders p
                  return (l1 ++ l2)
  
askToChoosePath :: FilePath -> IO Int
askToChoosePath p = do printList $ allEntries p
                       putStr "Your choise: "
                       result <- getLine
                       return $ read result

askToChoosePath' :: FilePath -> IO FilePath
askToChoosePath' p = do list <- allEntries p
                        printList $ return list
                        putStr "Your choise: "
                        result <- getLine
                        case readMaybe result of
                          Just r -> return $ list !! r
                          _      -> return p

askToChoosePath'' :: FilePath -> String -> IO FilePath
askToChoosePath'' p q = do list <- allEntries p
                           printList $ return list
                           putStr q
                           result <- getLine
                           case readMaybe result of
                             Just r -> return $ list !! r
                             _      -> return p


findSourceFile :: FilePath -> IO FilePath
findSourceFile p = do p_ <- askToChoosePath' p
                      ifM (doesFileExist p_)
                        (return p_)
                        (findSourceFile p_)

configFilePath :: String
configFilePath = "/home/alexander/.hspfm"
                        
--writeCOnfig :: IO String
readConfig = readFile configFilePath

menuEntries :: IO [String]
menuEntries = do text <- readConfig
                 return (lines text)

ui = do me <- menuEntries
        print "From config file:"
        printList $ return me
        result <- getLine
        case result of
          "q" -> print "quit"
          "h" -> mapM_ putStrLn ["Help:",
                                 "q - quit",
                                 "h - help",
                                 "a - add new entry",
                                 "[0..] - choose menu entry",
                                 ""]
                 >> ui
          "a" -> print "add new entry" >> addNewEntry
          _   -> case readMaybe result of
                   Just x -> if (x < length me)
                             then doEntry (me !! x) --print (me !! (x - 1))
                             else ui
                   _      -> ui
          
doEntry :: String -> IO ()
doEntry str = case length ws of --do print $ words str
                2 -> myCopyFile w1 w2
                _ -> print ("Failed to parse " ++ str)
                where
                  ws = words str
                  w1 = ws !! 0
                  w2 = ws !! 1

andM :: IO Bool -> IO Bool -> IO Bool
andM ioa iob = do a <- ioa
                  b <- iob
                  return (a && b)

myCopyFile :: FilePath -> FilePath -> IO ()
myCopyFile f1 f2 = ifM ((doesFileExist f1) `andM` (doesFileExist f2))
                   (copyFile f1 f2)
                   (print $ "Failed copy " ++ f1 ++ " -> " ++ f2)

addNewEntry :: IO ()
addNewEntry = print "Not implemented yet"


getModTime :: FilePath -> IO String
getModTime x@('/':_) = return x
getModTime p = do x <- getModificationTime p
                  let arr = words (show x)
                  let time = arr !! 1
                  return $ hms time
                    where
                      hms (a:b:':':d:e:':':g:k:_) = [a,b,':',d,e,':',g,k]
                      hms _ = "xx:xx:xx"
  
t = getModTime "/home/alexander/.hspfm"

data MenuEntry = CopyEntry {from :: FilePath,
                           to :: FilePath} |
                 SimpleIOEntry {text :: String,
                               function :: (String -> IO ())}
--  deriving (Show)

instance Show MenuEntry where
  show (CopyEntry from to) =
    "copyEntry   " ++ from ++ " -> " ++ to
  show (SimpleIOEntry text f) = "SimpleEntry " ++ text

data ModificationTime = ModificationTime {hh :: Int,
                                          mm :: Int,
                                          ss :: Int}
instance Show ModificationTime where
  show (ModificationTime h m s) = show h ++ ":" ++
                                  show m ++ ":" ++
                                  show s ++ ":"

showHelp :: String -> IO ()
showHelp str = print "This is Help message"

mainMenu :: [MenuEntry]
mainMenu = [SimpleIOEntry {text = "Quit",
                           function = (\s -> print $ "test function" ++ s)},
            SimpleIOEntry {text = "Help",
                           function = showHelp},
            CopyEntry {from = "/media/224/w40/test.txt",
                       to = "/home/alexander/tftpboot/test.txt"}]

configMenu :: IO [MenuEntry]
configMenu = do ls <- fmap lines readConfig
                return $ map lineToCopyEntry ls
                               
showMenu :: IO ()
showMenu = do cfg_menu <- configMenu
              mapM print (mainMenu ++ cfg_menu)
              return ()

                               
                               
lineToCopyEntry :: String -> MenuEntry
lineToCopyEntry str = CopyEntry from to
  where ws = words str
        from = ws !! 0
        to = ws !! 1

getModTime' :: FilePath -> IO ModificationTime
getModTime' p = do x <- getModificationTime p
                   let arr = words (show x)
                   let time = arr !! 1
                   return $ hms time
                     where
                       hms (a:b:':':c:d:':':e:f:_) =
                         ModificationTime (read [a,b] :: Int)
                                          (read [c,d] :: Int)
                                          (read [e,f] :: Int)
                       hms _ = ModificationTime 0 0 0
