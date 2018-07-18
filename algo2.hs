import System.IO
import Control.Conditional (ifM,condM)
import System.Directory
import System.FilePath.Posix
import System.Console.Terminal.Size
import Text.Read
import System.FilePath.Posix
import System.Console.ANSI
--import Turtle

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

data ModificationDate = ModificationDate {yy :: Int,
                                          mo :: Int,
                                          dd :: Int}
                        
data ModificationTime = ModificationTime {hh :: Int,
                                          mi :: Int,
                                          ss :: Int}
instance Show ModificationTime where
  show (ModificationTime h m s) = fillZero (show h) 2 ++ ":" ++
                                  fillZero (show m) 2 ++ ":" ++
                                  fillZero (show s) 2

instance Ord ModificationTime where
  compare (ModificationTime hl ml sl) (ModificationTime hr mr sr) =
    (toSeconds hl ml sl) `compare` (toSeconds hr mr sr)
    where toSeconds h m s = (h * 3600) + (m * 60) + s

instance Eq ModificationTime where
  (ModificationTime hl ml sl) == (ModificationTime hr mr sr) =
    (toSeconds hl ml sl) == (toSeconds hr mr sr)
    where toSeconds h m s = (h * 3600) + (m * 60) + s

instance Show ModificationDate where
  show (ModificationDate y m d) = fillZero (show y) 4 ++ "-" ++
                                  fillZero (show m) 2 ++ "-" ++
                                  fillZero (show d) 2

instance Ord ModificationDate where
  compare (ModificationDate yl ml dl) (ModificationDate yr mr dr) =
    (toDays yl ml dl) `compare` (toDays yr mr dr)
    where toDays y m d = (y * 360) + (m * 31) + d

instance Eq ModificationDate where
  (ModificationDate yl ml dl) == (ModificationDate yr mr dr) =
    (toDays yl ml dl) == (toDays yr mr dr)
    where toDays y m d = (y * 360) + (m * 31) + d
  

data FSEntry = FSFile {fs_id :: Int,
                       mod_time :: ModificationTime,
                       mod_date :: ModificationDate,
                       path :: FilePath} |
               FSDir {fs_id :: Int,
                      mod_time :: ModificationTime,
                      mod_date :: ModificationDate,
                      path :: FilePath}

instance Show FSEntry where
--  show (FSFile id time path) = "F" ++ (strId id) ++ "(" ++ (show time) ++ ") " ++ path
--  show (FSDir id time path) = "D" ++ (strId id) ++ "(" ++ (show time) ++ ") " ++ path
  show (FSFile id time date path) = "F" ++ (strId id) ++ (show date)++" "++(show time) ++" " ++ path
  show (FSDir id time date path) = "D" ++ (strId id) ++ (show date)++" "++(show time) ++ " " ++ path

fixStrLen :: String -> Int -> String
fixStrLen str len = if len > (length str)
                    then [' ' | x <- [1..l], True] ++ str
                    else cutStr len str
                      where
                        l = len - (length str)

fillStr :: Char -> String -> Int -> String
fillStr c str len = if len > (length str)
                    then [c | x <- [1..l], True] ++ str
                    else cutStr len str
  where
    l = len - (length str)

fillZero :: String -> Int -> String
fillZero = fillStr '0'

defaultId :: Int
defaultId = (-1)

defaultTime :: ModificationTime
defaultTime = ModificationTime 0 0 0

defaultDate :: ModificationDate
defaultDate = ModificationDate 0 0 0

strId :: Int -> String
strId (-1) = " "
strId x    = " " ++ (show x) ++ " "

doesPathExist :: FilePath -> IO Bool
doesPathExist p = do doesFileExist p >>= \fe -> doesDirectoryExist p >>= \de -> 
                       if fe || de then return True else return False

getModTime :: FilePath -> IO (ModificationTime, ModificationDate)
getModTime p = do doesPathExist p
                  >>= \exists -> case exists of
                                   False -> return [(show defaultDate), (show defaultTime)]
                                   True -> getModificationTime p >>= \x -> return $ words $ show x
                  >>= \arr -> return $ (hms (arr !! 1), ymd (arr !! 0))
                    where
                      hms (a:b:':':c:d:':':e:f:_) =
                        ModificationTime ((read [a,b] :: Int) + 3)
                                         (read [c,d] :: Int)
                                         (read [e,f] :: Int)
                      hms _ = defaultTime
                      ymd (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:_) =
                        ModificationDate (read [y1,y2,y3,y4] :: Int)
                                         (read [m1,m2] :: Int)
                                         (read [d1,d2] :: Int)
                      ymd _ = defaultDate



maybeCreateFSEntry :: FilePath -> IO (Maybe FSEntry)
maybeCreateFSEntry fp = do condM [(doesFileExist fp,
                                   getModTime fp >>= \(time, date) ->
                                      return . Just $ (FSFile defaultId time date fp)),
                                  (doesDirectoryExist fp,
                                   getModTime fp >>= \(time, date) ->
                                      return . Just $ (FSDir defaultId time date fp)),
                                  (return True, return Nothing)]

configFileName :: String
configFileName = ".hspfm"

maybeConfigFile :: IO (Maybe FSEntry)
maybeConfigFile = getHomeDirectory
  >>= \home_path -> maybeCreateFSEntry $ home_path ++ "/" ++ configFileName

findNearestFSEntry :: FilePath -> IO FSEntry
findNearestFSEntry fp = maybeCreateFSEntry fp
                        >>= \maybe_entry ->
                              case maybe_entry of
                                (Just x) -> return x
                                Nothing  -> findNearestFSEntry (takeDirectory fp)

createMenuEntry :: FSEntry -> FSEntry -> MenuEntry
createMenuEntry f@(FSFile _ _ _ _) t = CopyEntry defaultId f t
createMenuEntry f@(FSDir _ _ _ _) t = CopySelected defaultId f t

cfgLineToMenuEntry :: String -> IO MenuEntry
cfgLineToMenuEntry str = do let files = words str
                            (modt, modd) <- getModTime (files !! 1)
                            from <- findNearestFSEntry $ files !! 0
                            let to = (FSFile defaultId modt modd (files !! 1))
                            return (createMenuEntry from to)
                              
                              

menu :: IO [MenuEntry]
menu = do (Just cfg) <- maybeConfigFile
          content <- readFile (path cfg)
          mapM cfgLineToMenuEntry (lines content)

printMenu :: IO [MenuEntry] -> IO ()
printMenu menu = menu >>= \m -> mapM_ printMenuEntry m


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
     let td = \x -> (show $ mod_date x) ++ " " ++ (show $ mod_time x)
     case (modifiedField == "*") of
       True -> setSGR [SetColor Foreground Vivid Yellow]
       _ -> setSGR [SetColor Foreground Vivid Green]
     putStrLn $ (strId i) ++ ": " ++ (cutStr (w - 5) (path f))
     putStrLn $ "  => " ++ (cutStr w (path t))
     putStrLn $ "  => " ++ (td f) ++ " -> " ++ (td t) ++ " " ++ modifiedField
     putStrLn ['-' | x <- [1 .. (w)], True]
     setSGR [Reset]
     where
       modifiedField
         | ((mod_date f) == (mod_date t)) = 
             if ((mod_time f) > (mod_time t))
             then "*"
             else ""
         | ((mod_date f) > (mod_date t)) = "*"
         | True = ""
         
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
printMenuEnumed menu = menu >>= \m -> printMenu . return $ enumMenu m 0

maybeGetNum :: IO (Maybe Int)
maybeGetNum = getLine >>= \str -> return (readMaybe str :: Maybe Int)

drawLine :: IO ()
drawLine = termWidth
           >>= \tw -> putStrLn (['-' | x <- [1..(fromIntegral(tw - 2))], True])

start = do drawLine
           printMenuEnumed menu
           setSGR [SetColor Foreground Vivid Blue]
           putStr "Please select menu entry: "
           setSGR [Reset]
           choise <- maybeGetNum
           case choise of
             (Just n) -> menu >>= \m -> callMenuEntry (m !! n)
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
                    setFSId (FSFile _ mt md p) n_ = FSFile n_ mt md p
                    setFSId (FSDir _ mt md p) n_ = FSDir n_ mt md p

getAndPrintFSEnumed :: FSEntry -> ([FSEntry] -> [FSEntry]) -> IO [FSEntry]
getAndPrintFSEnumed fse filter_ = do entries <- listFSEntries (path fse)
                                     let filtered = filter_ entries
                                     let enumed = enumFS filtered 0
                                     putStrLn $ (path fse) ++ ":"
                                     mapM_ printFSEntry enumed
                                     return enumed

printFSEntry :: FSEntry -> IO ()
printFSEntry (FSFile i t d p) = do  
  tw <- termWidth
  let w = fromIntegral (tw - 26)
  putStrLn $ "F " ++ (strId i) ++ " " ++ (show d) ++ " " ++ (show t) ++ " "
    ++ (fixStrLen (takeFileName p) w)
printFSEntry (FSDir i t d p) = do
  tw <- termWidth
  let w = fromIntegral (tw - 26)
  putStrLn $ "D " ++ (strId i) ++ " " ++ (show t) ++ " " ++  (show t) ++ " "
    ++ (fixStrLen (takeFileName p) w)

-- test FS entry
tf :: IO FSEntry
tf = do getModTime "/home" >>= \(t, d) -> return (FSFile defaultId t d "/home")

askFile :: FSEntry -> ([FSEntry] -> [FSEntry]) -> IO FSEntry
askFile fse filter_ =
  getAndPrintFSEnumed fse filter_
  >>= \enumed -> putStr "Please select FS entry: "
  >> maybeGetNum
  >>= \choise -> case choise of
                   (Just n) -> case enumed !! n of
                                 (FSDir i t d p)  -> canonicalizePath p
                                                   >>= \cp -> askFile (FSDir i t d cp) filter_
                                 (FSFile _ _ _ p) -> print ("selected: " ++ p)
                                                   >> return (enumed !! n)
                   Nothing  -> askFile fse filter_

callMenuEntry :: MenuEntry -> IO ()
callMenuEntry (CopySelected id_ f t) =
  askFile f (filterFSEtriesByExt (takeExtension (path t)))
  >>= \ff -> copyFile' ff t  
callMenuEntry (CopyEntry id_ f t) = do copyFile' f t

copyFile' :: FSEntry -> FSEntry -> IO ()
copyFile' f@(FSFile _ _ _ _) t@(FSFile _ _ _ _) =
  putStrLn ("copy: " ++ (show f) ++ " -> " ++ (show t))
  >> copyFile (path f) (path t)
copyFile' f@(FSFile _ _ _ fp) (FSDir ti tt td tp) =
  canonicalizePath tpath
  >>= \new_tp -> putStrLn ("copy: " ++ (show f) ++ " -> " ++ new_tp)
  >> copyFile (path f) new_tp
  where
    tpath = tp ++ "/" ++ (takeFileName fp)

filterFSEtriesByExt :: String -> [FSEntry] -> [FSEntry]
filterFSEtriesByExt ext entries = filter predicat entries
  where
    predicat :: FSEntry -> Bool
    predicat (FSFile _ _ _ p)
      | (ext == takeExtension p) = True
      | True                     = False
    predicat _ = False
  
t = do d <- tf
       askFile d (filterFSEtriesByExt ".epk")

--sord (Ord instance...)
-- new menuEntry: Dir -> Dir = select from, select to file name
-- add data to modTime
