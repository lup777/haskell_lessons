--words.hs
import System.IO

import Prelude hiding (putStr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BU

data RuString = RuString String

data Entry = Entry {english :: [String],
                    local   :: [RuString],
                    synonym :: [String]}
  deriving (Show)

instance Show RuString where
  show (RuString str) = BU.toString . BU.fromString $ str


vac :: [Entry]
vac = [(Entry ["have a go"] [RuString "попытаться"] ["try"]),
       (Entry ["have a go"] [RuString "попытаться"] ["try"])]
main = putStrLn $ show vac
