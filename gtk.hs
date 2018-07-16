import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

main :: IO()
main = do
  void initGUI
  window <- windowNew
  hbox   <- hBoxNew False 3
  vbox   <- vBoxNew False 3
  button1 <- buttonNewWithLabel "Button 1"
  button2 <- buttonNewWithLabel "Button 2"
  aspectFrame <- aspectFrameNew 0.5 0.5 (Just 3.0)
  frameSetLabel aspectFrame "Aspect Ratio 3.0"
  frameSetLabelAlign aspectFrame 1.0 0.0
  set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
              containerBorderWidth := 10, containerChild := vbox]
  boxPackStart vbox hbox PackNatural 3
  boxPackStart hbox button1 PackNatural 3
  boxPackStart hbox button2 PackNatural 3
  boxPackStart vbox aspectFrame PackNatural 3
  
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
