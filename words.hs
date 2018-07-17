--words.hs

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import System.IO

import Prelude hiding (putStr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BU
import System.Random
import Data.List
import Control.Monad.State

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
vac = [(Entry ["have a go"] [RuString "попытаться"] [RuString "try"]),
       (Entry ["come up with"] [RuString "придумывать"] [RuString ""]),
       (Entry ["get on"] [RuString "садиться на, садиться в транспортное средство",
                          RuString "звонить/поднять трубку",
                          RuString "двигаться дальше",
                          RuString "достигать"] [RuString ""]),
       (Entry ["get across"] [RuString "доступно объяснять"] [RuString "she can get across even the most complicated rule"]),
       (Entry ["get through to"] [RuString "связываться по телефону"] [RuString "I can't get through to them. Не могу до них дозвониться"]),
       (Entry ["be out"] [RuString "отсутствовать"] [RuString ""]),
       (Entry ["be off"] [RuString "уезжать",
                          RuString "уходить",
                          RuString "быть не работающим, свобоюным"] [RuString ""]),
       (Entry ["be over"] [RuString "завершиться",
                           RuString "окончиться"] [RuString ""]),
       (Entry ["be up to"] [RuString "намереваться",
                            RuString "собираться что-то сделать",
                            RuString "зависеть от"] [RuString ""])]

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
showResult entry res vac = if res then putStrLn $ "You are Right :-)   -> " ++ (show entry) -- ++ (show vac)
                           else        putStrLn $ "You are not Right   -> " ++ (show entry) -- ++ (show vac)

without :: [Entry] -> Entry -> [Entry]
without [] entry = []
without (x:xs) entry
  | x == entry = xs `without` entry
  | otherwise  = [x] ++ (xs `without` entry)

main = learnWerbs vac

-- https://preply.com/blog/2016/02/02/90-anglijskih-frazovyh-glagolov-na-vse-sluchai-zhizni/



---- SANDER -----

counter1 :: State Int String
counter1 = do modify (+1)
              get >>= \a -> return $ "counter1 = " ++ (show a)


t :: IO ()
t = do putStrLn . show $ evalState counter1 0
       putStrLn . show $ evalState counter1 0

