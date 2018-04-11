import System.IO
import Control.Conditional (ifM,condM)
import System.Directory
import System.FilePath.Posix
import System.Console.Terminal.Size
import Text.Read

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
  show (ModificationTime h m s) = show h ++ ":" ++
                                  show m ++ ":" ++
                                  show s

data FSEntry = FSFile {fs_id :: Int,
                       mod_time :: ModificationTime,
                       path :: FilePath} |
               FSDir {fs_id :: Int,
                      mod_time :: ModificationTime,
                      path :: FilePath}

instance Show FSEntry where
  show (FSFile id time path) = "F" ++ (strId id) ++ "(" ++ (show time) ++ ") " ++ path
  show (FSDir id time path) = "D" ++ (strId id) ++ "(" ++ (show time) ++ ") " ++ path

defaultId :: Int
defaultId = (-1)

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
                            to   <- findNearestFSEntry $ files !! 1
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
printMenuEntry (CopyEntry i f t) = do tw <- termWidth
                                      let w = fromIntegral (tw - 2)
                                      putStrLn $ (strId i) ++ ": " ++
                                        (cutStr (w - 5) (path f))
                                      putStrLn $ "  => "
                                        ++ (cutStr w (path t))
                                      putStrLn $ "  => " ++ (show $ mod_time f)
                                        ++ " -> " ++ (show $ mod_time t)
                                      putStrLn ['-' | x <- [1 .. (w)], True]
printMenuEntry (CopySelected i f t) = do tw <- termWidth
                                         let w = fromIntegral (tw - 2)
                                         putStrLn $ (strId i) ++ ": " ++
                                           (cutStr (w - 5) (path f))
                                         putStrLn $ "  => "
                                           ++ (cutStr w (path t))
                                         putStrLn $ "  => " ++ (show $ mod_time f)
                                           ++ " -> " ++ (show $ mod_time t)
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

start = do printMenuEnumed menu
           choise <- maybeGetNum
           putStr "Please select menu entry: "
           case choise of
             (Just n) -> do m <- menu
                            callMenuEntry (m !! n)
             Nothing -> return ()
           start

callMenuEntry :: MenuEntry -> IO ()
callMenuEntry e = return ()

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

t = do entries <- listFSEntries "/home"
       mapM_ (putStrLn . show) entries
