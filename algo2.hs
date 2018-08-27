import System.IO
import Control.Conditional (ifM,condM)
import System.Directory
import System.FilePath.Posix
import System.Console.Terminal.Size
import Text.Read
import System.FilePath.Posix
import System.Console.ANSI
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Check (checkSecurity)
import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BS
import System.Process
--import Turtle

data MenuEntry = CopyEntry {entry_id :: Int,
                            from :: FSEntry,
                            to :: FSEntry} |
                 SimpleIOEntry {entry_id :: Int,
                                text :: String,
                                function :: (String -> IO ())} |
                 CopySelected {entry_id :: Int,
                               from :: FSEntry,
                               to :: FSEntry} |
                 TarEntry {entry_id :: Int,
                            from :: FSEntry,
                            to :: FSEntry,
                            ftype :: String} |
                 TarEntrySelected {entry_id :: Int,
                                   from :: FSEntry,
                                   to :: FSEntry,
                                   ftype :: String}
  --  deriving (Show)

instance Show MenuEntry where
  show (CopyEntry id from to) =
    (strId id) ++ (show from) ++ " -> " ++ (show to)
  show (SimpleIOEntry id text f) = (strId id) ++ text
  show (CopySelected id from to) =
    (strId id) ++ (show from) ++ " -> " ++ (show to)
  show (TarEntry id from to ft) =
    (strId id) ++ (show from) ++ " -> " ++ (show to) ++ " (" ++ ft ++ ")"
  show (TarEntrySelected id from to ft) =
    (strId id) ++ (show from) ++ " -> " ++ (show to) ++ " (" ++ ft ++ ")"

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

createCopyMenuEntry :: FSEntry -> FSEntry -> MenuEntry
createCopyMenuEntry f@(FSFile _ _ _ _) t = CopyEntry defaultId f t
createCopyMenuEntry f@(FSDir _ _ _ _) t = CopySelected defaultId f t

createMenuEntry :: [String] -> IO MenuEntry
createMenuEntry fields = do 
  case fields of
    ("tar":xs) -> do (modt, modd) <- getModTime (fields !! 2)
                     from <- findNearestFSEntry $ fields !! 1
                     let to = (FSFile defaultId modt modd (fields !! 2))
                     return $ createTarMenuEntry from to (fields !! 3)
    othervise -> do (modt, modd) <- getModTime (fields !! 1)
                    from <- findNearestFSEntry $ fields !! 0
                    let to = (FSFile defaultId modt modd (fields !! 1))
                    return $ createCopyMenuEntry from to
                              
                                    
createTarMenuEntry :: FSEntry -> FSEntry -> String -> MenuEntry
createTarMenuEntry f@(FSFile _ _ _ _) t filter = TarEntry defaultId f t filter
createTarMenuEntry f@(FSDir _ _ _ _) t filter= TarEntrySelected defaultId f t filter

cfgLineToMenuEntry :: String -> IO MenuEntry
cfgLineToMenuEntry str = do createMenuEntry $ words str
                                
                              

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
       modifiedField = case whetherFSEntryNeedsUpdate f t of
                         True -> "*"
                         otherwise -> ""

printMenuEntry (CopySelected i f t) = printMenuEntry (CopyEntry i f t)

printMenuEntry (TarEntrySelected i f t ft) =
  do w <- terminalWidth
     case (modifiedField == "*") of
       True -> setSGR [SetColor Foreground Vivid Yellow]
       _ -> setSGR [SetColor Foreground Vivid Green]
     putStrLn $ (strId i) ++ ": TAR ("++ ft ++ ")" ++ (cutStr (w - 5) (path f))
     putStrLn $ "  => " ++ (cutStr w (path t))
     putStrLn $ "  => " ++ (showModDateTime f) ++ " -> "
       ++ (showModDateTime t) ++ " " ++ modifiedField
     putStrLn ['-' | x <- [1 .. (w)], True]
     setSGR [Reset]
       where
         showModDateTime f = (show $ mod_date f) ++ " " ++ (show $ mod_time f)
         modifiedField = case whetherFSEntryNeedsUpdate f t of
                           True -> "*"
                           otherwise -> ""

whetherFSEntryNeedsUpdate :: FSEntry -> FSEntry -> Bool
whetherFSEntryNeedsUpdate f t
  | ((mod_date f) == (mod_date t)) = 
      if ((mod_time f) > (mod_time t))
      then True
      else False
  | ((mod_date f) > (mod_date t)) = True
  | otherwise = False

terminalWidth :: IO Int
terminalWidth = do tw <- termWidth
                   return $ fromIntegral (tw - 2)

setMenuEtryId :: MenuEntry -> Int -> MenuEntry
setMenuEtryId (CopyEntry i f t) id_ = CopyEntry id_ f t
setMenuEtryId (CopySelected i f t) id_ = CopySelected id_ f t
setMenuEtryId (TarEntrySelected i f t ft) id_ = TarEntrySelected id_ f t ft

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
  askFile f (filterFSEntriesByExt (takeExtension (path t)))
  >>= \ff -> copyFile' ff t  
callMenuEntry (CopyEntry id_ f t) = do copyFile' f t
callMenuEntry (TarEntrySelected id_ f t ft) =
  --do ff <- askFile f (filterFSEntriesByExt (takeExtension (path t)))
  do ff <- askFile f (filterFSEntriesByExt ft)
     let to = takeDirectory (path t)
     putStrLn $ to ++ " -> " ++ (path ff)
     --Tar.unpack to . Tar.read . GZip.decompress =<< BS.readFile (path ff)
     -- archive contains absolute pathes :-(
     let rmCmd = "sudo rm -Rf " ++ to ++ "/*"
     putStrLn $ "Need ROOT passwd for: " ++ rmCmd
     callCommand rmCmd
     extractTarGz (path ff) to

extractTarGz :: FilePath -> FilePath -> IO ()
extractTarGz from to = do
  setCurrentDirectory to
  callCommand $ "tar -zxvf " ++ from

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

filterFSEntriesByExt :: String -> [FSEntry] -> [FSEntry]
filterFSEntriesByExt ext entries = filter predicat entries
  where
    predicat :: FSEntry -> Bool
    predicat (FSFile _ _ _ p)
      | (ext == takeExtension p) = True
      | True                     = False
    predicat _ = False
  
t = do d <- tf
       askFile d (filterFSEntriesByExt ".epk")

--sord (Ord instance...)
-- new menuEntry: Dir -> Dir = select from, select to file name
-- add data to modTime
