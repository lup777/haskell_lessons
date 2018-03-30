{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- will use brick
-- https://samtay.github.io/articles/brick.html
module Main where

#if !(MIN_VERSION_base(4,11,0))
--import Data.Monoid
#endif

import Brick.Main
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( (<+>)
--  , (<=>)
  , padLeft
--  , hyperlink
--  , modifyDefAttr
  , str
  )
import qualified Graphics.Vty as V
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)
--import Brick.Markup (markup, (@?))
--import Data.Text.Markup ((@@))
--import System.Environment

--import Algo

ui :: Widget ()
--ui =
--    vBox [ str "Yo!"
--         , (withAttr "foundFgOnly" $
--           str ("How do You do?")
--           <=> str "writting?")]
--         , hBox [ str "Yo another time!"
--                , str "he he ^_^)" ]]

--ui = (m1 <=> m2) <+> (padLeft (Pad 20) m3) <+> str "hi!"
--    where
--        m1 = markup $ ("Hello" @@ fg V.blue) <> ", " <> ("world!" @@ fg V.red)
--        m2 = markup $ ("Hello" @? "keyword1") <> ", " <> ("world!" @? "keyword2")
--        m3 = markup $ ("Hello," @? "keyword1") <> "\n" <> ("world!" @? "keyword2")

ui = str "Hi!" <+> (padLeft (Pad 20) (str "Yo!"))

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ ("keyword1",      fg V.magenta)
  , ("keyword2",      V.white `on` V.blue)
  ]


app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()


-- cmd line params
-- main = getArgs >>= print . head


