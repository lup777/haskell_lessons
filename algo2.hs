import System.IO
import Control.Conditional (ifM,condM)
import System.Directory
import System.FilePath.Posix
import System.Console.Terminal.Size
import Text.Read
import System.FilePath.Posix

data MenuEntry = CopyEntry {entry_id :: Int,
                            from :: FSEntry,
                            to :: FSEntry} |
                 SimpleIOEntry {entry_id :: Int,
                                text :: String,
                                function :: (String -> IO ())} |
                 CopySelected {emtry_id :: Int,
                               from :: FSEntry,
                               to :: FSEntry}
--  deriving (Show)

instance Show MenuEntry where
  show (CopyEntry id from to) =
    (strId id) ++ (show from) ++ " -> " ++ (show to)
  show (SimpleIOEntry id text f) = (strId id) ++ text
  show (CopySelected id from to) =
    (strId id) ++ (show from) ++ " -> " ++ (show to)

data ModificationTime = ModificationTime {hh :: Int,
                                          mm :: Int,
                                          ss :: Int}
instance Show ModificationTime where
  show (ModificationTime h m s) = fixStrLen (show h) 2 ++ ":" ++
                                  fixStrLen (show m) 2 ++ ":" ++
                                  fixStrLen (show s) 2

data FSEntry = FSFile {fs_id :: Int,
                       mod_time :: ModificationTime,
                       path :: FilePath} |
               FSDir {fs_id :: Int,
                      mod_time :: ModificationTime,
                      path :: FilePath}

instance Show FSEntry where
--  show (FSFile id time path) = "F" ++ (strId id) ++ "(" ++ (show time) ++ ") " ++ path
--  show (FSDir id time path) = "D" ++ (strId id) ++ "(" ++ (show time) ++ ") " ++ path
  show (FSFile id time path) = "F" ++ (strId id) ++ (show time) ++ " " ++ path
  show (FSDir id time path) = "D" ++ (strId id) ++ (show time) ++ " " ++ path

fixStrLen :: String -> Int -> String
fixStrLen str len = if len > (length str)
                    then [' ' | x <- [1..l], True] ++ str
                    else cutStr len str
                      where
                        l = len - (length str)

defaultId :: Int
defaultId = (-1)

defaultTime :: ModificationTime
defaultTime = ModificationTime 0 0 0

strId :: Int -> String
strId (-1) = " "
strId x    = " " ++ (show x) ++ " "

getModTime :: FilePath -> IO ModificationTime
getModTime p = do x <- getModificationTime p
                  let arr = words (show x)
                  let time = arr !! 1
                  return $ hms time
                    where
                      hms (a:b:':':c:d:':':e:f:_) =
                        ModificationTime (read [a,b] :: Int)
                                         (read [c,d] :: Int)
                                         (read [e,f] :: Int)
                      hms _ = ModificationTime 0 0 0


maybeCreateFSEntry :: FilePath -> IO (Maybe FSEntry)
maybeCreateFSEntry fp = do condM [(doesFileExist fp,
                                   do time <- getModTime fp
                                      return . Just $ (FSFile defaultId time fp)),
                                  (doesDirectoryExist fp,
                                   do time <- getModTime fp
                                      return . Just $ (FSDir defaultId time fp)),
                                  (return True, return Nothing)]

configFileName :: String
configFileName = ".hspfm"

maybeConfigFile :: IO (Maybe FSEntry)
maybeConfigFile = do home_path <- getHomeDirectory
                     maybeCreateFSEntry $ home_path ++ "/" ++ configFileName

findNearestFSEntry :: FilePath -> IO FSEntry
findNearestFSEntry fp = do maybe_entry <- maybeCreateFSEntry fp
                           case maybe_entry of
                             (Just x) -> return x
                             Nothing  -> findNearestFSEntry (takeDirectory fp)

createMenuEntry :: FSEntry -> FSEntry -> MenuEntry
createMenuEntry f@(FSFile _ _ _) t = CopyEntry defaultId f t
createMenuEntry f@(FSDir _ _ _) t = CopySelected defaultId f t

cfgLineToMenuEntry :: String -> IO MenuEntry
cfgLineToMenuEntry str = do let files = words str
                            from <- findNearestFSEntry $ files !! 0
                            let to = (FSFile defaultId defaultTime (files !! 1))
                            return (createMenuEntry from to)

menu :: IO [MenuEntry]
menu = do (Just cfg) <- maybeConfigFile
          content <- readFile (path cfg)
          mapM cfgLineToMenuEntry (lines content)

printMenu :: IO [MenuEntry] -> IO ()
printMenu menu = do m <- menu
                    mapM_ printMenuEntry m


cutStr :: Int -> String -> String
cutStr n str = drop (len - n) str
               where
                 len = length str

termWidth :: IO Integer
termWidth = do s <- size
               case s of
                 (Just a) -> if (width a == 0) then (return 80) else (return $ width a)
                 Nothing  -> return 80


printMenuEntry :: MenuEntry -> IO ()
printMenuEntry (CopyEntry i f t) =
  do tw <- termWidth
     let w = fromIntegral (tw - 2)
     putStrLn $ (strId i) ++ ": " ++ (cutStr (w - 5) (path f))
     putStrLn $ "  => " ++ (cutStr w (path t))
     putStrLn $ "  => " ++ (show $ mod_time f) ++ " -> " ++ (show $ mod_time t)
     putStrLn ['-' | x <- [1 .. (w)], True]
printMenuEntry (CopySelected i f t) =
  do tw <- termWidth
     let w = fromIntegral (tw - 2)
     putStrLn $ (strId i) ++ ": " ++ (cutStr (w - 5) (path f))
     putStrLn $ "  => " ++ (cutStr w (path t))
     putStrLn $ "  => " ++ (show $ mod_time f) ++ " -> " ++ (show $ mod_time t)
     putStrLn ['-' | x <- [1 .. (w)], True]

setMenuEtryId :: MenuEntry -> Int -> MenuEntry
setMenuEtryId (CopyEntry i f t) id_ = CopyEntry id_ f t
setMenuEtryId (CopySelected i f t) id_ = CopyEntry id_ f t

enumMenu :: [MenuEntry] -> Int -> [MenuEntry]
enumMenu [] _     = []
enumMenu (x:xs) n = (setMenuEtryId x n) : (enumMenu xs (n+1))

printMenuEnumed :: IO [MenuEntry] -> IO ()
printMenuEnumed menu = do m <- menu
                          printMenu . return $ enumMenu m 0

maybeGetNum :: IO (Maybe Int)
maybeGetNum = do str <- getLine
                 return (readMaybe str :: Maybe Int)

drawLine :: IO ()
drawLine = do tw <- termWidth
              putStrLn (['-' | x <- [1..(fromIntegral(tw - 2))], True])

start = do drawLine
           printMenuEnumed menu
           putStr "Please select menu entry: "
           choise <- maybeGetNum
           case choise of
             (Just n) -> do m <- menu
                            callMenuEntry (m !! n)
             Nothing -> return ()
           start

theOnly :: [Maybe a] -> [a]
theOnly []     = []
theOnly (x:xs) = case x of
                   (Just e) -> e : theOnly xs
                   Nothing  -> theOnly xs

--maybeCreateFSEntry
listFSEntries :: FilePath -> IO [FSEntry]
listFSEntries fp = do list <- getDirectoryContents fp
                      maybe_entries <- mapM maybeCreateFSEntry (map ((fp ++ "/") ++) list)
                      return (theOnly maybe_entries)

enumFS :: [FSEntry] -> Int -> [FSEntry]
enumFS [] _     = []
enumFS (x:xs) n = (setFSId x n) : (enumFS xs (n+1))
                  where
                    setFSId (FSFile _ m p) n_ = FSFile n_ m p
                    setFSId (FSDir _ m p) n_ = FSDir n_ m p

getAndPrintFSEnumed :: FSEntry -> ([FSEntry] -> [FSEntry]) -> IO [FSEntry]
getAndPrintFSEnumed fse filter_ = do entries <- listFSEntries (path fse)
                                     let filtered = filter_ entries
                                     let enumed = enumFS filtered 0
                                     putStrLn $ (path fse) ++ ":"
                                     mapM_ printFSEntry enumed
                                     return enumed

printFSEntry :: FSEntry -> IO ()
printFSEntry (FSFile i t p) = do  
  tw <- termWidth
  let w = fromIntegral (tw - 15)
  putStrLn $ "F " ++ (strId i) ++ " " ++ (show t) ++ " "
    ++ (fixStrLen (takeFileName p) w)
printFSEntry (FSDir i t p) = do
  tw <- termWidth
  let w = fromIntegral (tw - 15)
  putStrLn $ "D " ++ (strId i) ++ " " ++ (show t) ++ " "
    ++ (fixStrLen (takeFileName p) w)

-- test FS entry
tf :: IO FSEntry
tf = do t <- (getModTime "/home")
        return (FSFile defaultId t "/home")

askFile :: FSEntry -> ([FSEntry] -> [FSEntry]) -> IO FSEntry
askFile fse filter_ =
  do enumed <- getAndPrintFSEnumed fse filter_
     putStr "Please select FS entry: "
     choise <- maybeGetNum
     case choise of
       (Just n) -> case enumed !! n of
                     (FSDir i t p)  -> do clear_path <- canonicalizePath p
                                          askFile (FSDir i t clear_path) filter_
                     (FSFile _ _ p) -> print ("selected: " ++ p)
                                       >> return (enumed !! n)
       Nothing  -> askFile fse filter_

callMenuEntry :: MenuEntry -> IO ()
callMenuEntry (CopySelected id_ f t) =
  do ff <- askFile f (filterFSEtriesByExt (takeExtension (path t)))
     copyFile' ff t  
callMenuEntry (CopyEntry id_ f t) = do copyFile' f t

copyFile' :: FSEntry -> FSEntry -> IO ()
copyFile' f@(FSFile _ _ _) t@(FSFile _ _ _) =
  putStrLn ("copy: " ++ (show f) ++ " -> " ++ (show t))
  >> copyFile (path f) (path t)
copyFile' f@(FSFile _ _ fp) (FSDir ti tt tp) =
  do new_tp <- canonicalizePath tpath
     putStrLn ("copy: " ++ (show f) ++ " -> " ++ new_tp)
     copyFile (path f) new_tp
     where
       tpath = tp ++ "/" ++ (takeFileName fp)

filterFSEtriesByExt :: String -> [FSEntry] -> [FSEntry]
filterFSEtriesByExt ext entries = filter predicat entries
  where
    predicat :: FSEntry -> Bool
    predicat (FSFile _ _ p)
      | (ext == takeExtension p) = True
      | True                     = False
    predicat _ = False
  
t = do d <- tf
       askFile d (filterFSEtriesByExt ".epk")

--sord (Ord instance...)
-- new menuEntry: Dir -> Dir = select from, select to file name
-- add data to modTime
