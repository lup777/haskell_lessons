--words.hs

import System.IO

import Prelude hiding (putStr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BU
import System.Random
import Data.List
import Control.Monad.State
import System.Console.ANSI

data RuString = RuString String

data Entry = Entry {english :: [String],
                    local   :: [RuString],
                    synonym :: [RuString]}
  deriving (Show)

instance Show RuString where
  show (RuString str) = BU.toString . BU.fromString $ str

instance Eq Entry where
  (Entry e l s) == (Entry e1 l1 s1) = (e == e1 && l == l1 && s == s1 )

instance Eq RuString where
  (RuString s) == (RuString s1) = s == s1

vac :: [Entry]
vac = [ Entry ["have a go"] [RuString "попытаться"] [RuString "try"]
      , Entry ["come up with"] [RuString "придумывать"] [RuString ""]
      , Entry ["get on"] [ RuString "садиться на, садиться в транспортное средство"
                         , RuString "звонить/поднять трубку"
                         , RuString "двигаться дальше"
                         , RuString "достигать"] [RuString ""]
      , Entry ["get across"] [RuString "доступно объяснять"] [RuString "she can get across even the most complicated rule"]
      , Entry ["get through to"] [RuString "связываться по телефону"] [RuString "I can't get through to them. Не могу до них дозвониться"]
      , Entry ["be out"] [ RuString "отсутствовать"] [RuString ""]
      , Entry ["be off"] [ RuString "уезжать"
                         , RuString "уходить"
                         , RuString "быть не работающим, свободным"] [RuString ""]
      , Entry ["be over"] [ RuString "завершиться"
                          , RuString "окончиться"] [RuString ""]
      , Entry ["be up to"] [ RuString "намереваться"
                           , RuString "собираться что-то сделать"
                           , RuString "зависеть от"] [RuString ""]
      , Entry ["pull out"] [ RuString "вытаскивать (из сумки), выходить (из боя, предприятия)"] [RuString]
      , Entry ["pass by"] [RuString "проходить мимо"] [RuString ""]] ++ conditionals

conditionals :: [Entry]
conditionals = [ Entry ["If you heat water, it boils"] [RuString "Если вы нагреваете воду, она кипит"] [RuString "0 type"]
               , Entry ["If we finish later, I'll have to call and cancel my next lesson"] [RuString "Если мы закончим позже, мне придётся позвонить и отменить мой следующий урок"] [RuString "1 type"]
               , Entry ["If I had a lot of money, I would buy a yacht"] [RuString "Если бы у меня было много денег (сейчас), я бы купил яхту"] [RuString "2 type"]
               , Entry ["If I had had a lot of money, I would have bought a yacht"] [RuString "Если бы у меня было много денег (в прошлом), я бы купил яхту"] [RuString "3 type"]
               , Entry ["If I had entered ITMO, I wouldn't be English teacher"] [RuString "Если бы я поступил в ИТМО, я бы не стал учителем английского языка"] [RuString "mixed type (4)"]
               , Entry ["If I liked death metal, I would have bought tickets to that concert last weak"] [RuString "Если ли бы я любил дэд метал (вообще), я бы купил билет на тот концерт на прошлой неделе"]
                 [RuString "mixed type (4)"]
               , Entry ["If he had crashed the plane in a town, he would probably have killed someone"] [RuString "Если бы он разбил самалёт в городе, он, возможно, убил бы кого-нибудь"] [RuString "2 type"]
               ]

maybeGetRandomVacEntry :: [Entry] -> IO (Maybe Entry)
maybeGetRandomVacEntry [] = return Nothing
maybeGetRandomVacEntry vac_ = (randomRIO (0, (length vac_) - 1 ) :: IO Int)
                         >>= \num -> return . Just $ (vac_ !! num)

--checkRandomWord :: [Entry] -> IO Bool
--checkRandomWord vac_ = getRandomVacEntry vac_ >>= checkWord

checkWord :: Entry -> IO Bool
checkWord entry = checkEnglish entry
--  \result -> if result == True then putStrLn $ "You are Right :-)   -> " ++ (show entry)
--             else                   putStrLn $ "You are not Right   -> " ++ (show entry)
--  >> return result

checkEnglish :: Entry -> IO Bool
checkEnglish entry = requestEnglish entry >>= \answer -> return $ answer `isInVac` (english entry)

requestEnglish :: Entry -> IO String
requestEnglish entry = do putStr ("== " ++ (show . local $ entry ) ++ " ? ")
                          getLine

isInVac :: String -> [String] -> Bool
isInVac str [] = False
isInVac str (x:xs)
  | (isSubsequenceOf str x) == True = True
  | otherwise                       = isInVac str xs 
  
learnWerbs :: [Entry] -> IO ()
learnWerbs []   = putStrLn "GoodBy!"
learnWerbs vac_ = maybeGetRandomVacEntry vac_ >>=
                  \entry -> case entry of
                              (Just e) -> do isCorrect <- checkWord e
                                             showResult e isCorrect vac_
                                             if isCorrect then learnWerbs $ vac_ `without` e
                                             else              learnWerbs vac_
                              Nothing -> return ()

showResult :: Entry -> Bool -> [Entry] -> IO ()
showResult entry res vac = if res then do printYellow $ "You are Right :-)   -> " ++ (show entry)
                           else           printRed $ "You are not Right   -> " ++ (show entry)

without :: [Entry] -> Entry -> [Entry]
without [] entry = []
without (x:xs) entry
  | x == entry = xs `without` entry
  | otherwise  = [x] ++ (xs `without` entry)

main = do learnWerbs vac

printYellow :: String -> IO ()
printYellow str = do setSGR [SetColor Foreground Vivid Yellow]
                     --setSGR [SetColor Background Vivid Blue]
                     putStrLn str
                     setSGR [Reset]

printRed :: String -> IO ()
printRed str = do setSGR [SetColor Foreground Vivid Red]
                  putStrLn str
                  setSGR [Reset]

-- https://preply.com/blog/2016/02/02/90-anglijskih-frazovyh-glagolov-na-vse-sluchai-zhizni/



---- SANDER -----

counter1 :: State Int String
counter1 = do modify (+1)
              get >>= \a -> return $ "counter1 = " ++ (show a)


t :: IO ()
t = do putStrLn . show $ evalState counter1 0
       putStrLn . show $ evalState counter1 0

