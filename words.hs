--words.hs
import System.IO

import Prelude hiding (putStr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BU
import System.Random
import Data.List

data RuString = RuString String

data Entry = Entry {english :: [String],
                    local   :: [RuString],
                    synonym :: [String]}
  deriving (Show)

instance Show RuString where
  show (RuString str) = BU.toString . BU.fromString $ str


vac :: [Entry]
vac = [(Entry ["have a go"] [RuString "попытаться"] ["try"]),
       (Entry ["come up with"] [RuString "придумывать"] [""]),
       (Entry ["get on"] [RuString "садиться на, садиться в транспортное средство",
                          RuString "звонить/поднять трубку",
                          RuString "двигаться дальше",
                          RuString "достигать"] [""])]

getRandomVacEntry :: IO Entry
getRandomVacEntry = (randomRIO (0, (length vac) - 1 ) :: IO Int) >>= \num -> return (vac !! num)



checkRandomWord :: IO ()
checkRandomWord = getRandomVacEntry >>= checkWord

checkWord :: Entry -> IO ()
checkWord entry = checkEnglish entry >>=
  \result -> if result == True then putStrLn $ "You are Right :-)   -> " ++ (show entry)
             else                   putStrLn $ "You are not Right   -> " ++ (show entry)

checkEnglish :: Entry -> IO Bool
checkEnglish entry = requestEnglish entry >>= \answer -> return $ isInVac answer $ english entry

requestEnglish :: Entry -> IO String
requestEnglish entry = do putStr ("== " ++ (show . local $ entry ) ++ " ? ")
                          getLine

isInVac :: String -> [String] -> Bool
isInVac str [] = False
isInVac str (x:xs)
  | (isSubsequenceOf str x) == True = True
  | otherwise                       = isInVac str xs 
  
main = checkRandomWord >> putStrLn ""

