{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Error.Util (failWithM)
import Control.Exception (bracket)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Gdk
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk
import qualified Graphics.X11.Xlib.Atom as X11
  ( internAtom,
  )
import qualified Graphics.X11.Xlib.Display as X11
  ( closeDisplay,
    defaultRootWindow,
    openDisplay,
  )
import qualified Graphics.X11.Xlib.Extras as X11
import qualified Graphics.X11.Xlib.Types as X11 (Display)
import System.Process
  ( CreateProcess (..),
    createProcess,
    proc,
  )

data PanelError = NoDisplay | NoMonitor
  deriving (Show)

padding :: Word32
padding = 4

runApp :: String -> IO ()
runApp app = do
  _ <- createProcess (proc app [])
  pure ()

runTerminal :: IO ()
runTerminal = runApp "xfce4-terminal"

runHoodle :: IO ()
runHoodle = runApp "hoodle"

runChrome :: IO ()
runChrome = runApp "google-chrome-stable"

addWorkspaceButton :: Gtk.Box -> Int -> IO ()
addWorkspaceButton box n = do
  button <- new Gtk.Button [#label := T.pack (show n)]
  on button #clicked (pure ())
  #packStart box button False False padding

getNumberOfWorkspaces :: X11.Display -> IO Int
getNumberOfWorkspaces x11disp = do
  atom <- X11.internAtom x11disp "_NET_NUMBER_OF_DESKTOPS" True
  let rwin = X11.defaultRootWindow x11disp
  mval <- X11.getWindowProperty32 x11disp atom rwin
  let result = fromIntegral $ fromMaybe 1 $ join (fmap listToMaybe mval)
  pure result

main :: IO ()
main = do
  bracket (X11.openDisplay "") (X11.closeDisplay) $ \x11disp -> do
    numWorkspaces <-
      (\n -> if n > 10 then 10 else n) <$> getNumberOfWorkspaces x11disp
    e <- runExceptT $ do
      liftIO $ Gtk.init Nothing
      display <- failWithM NoDisplay Gdk.displayGetDefault
      monitor <- failWithM NoMonitor (#getPrimaryMonitor display)
      liftIO $ do
        rect <- #getGeometry monitor
        width <- get rect #width
        window <-
          new
            Gtk.Window
            [ #title := "panel-for-pen",
              #gravity := Gdk.GravityNorthWest,
              #decorated := False
            ]
        #setSizeRequest window width 30
        #stick window
        on window #destroy Gtk.mainQuit
        box <- new Gtk.Box []

        buttonTerminal <-
          new Gtk.Button [#label := "Terminal"]
        on buttonTerminal #clicked runTerminal
        buttonHoodle <-
          new Gtk.Button [#label := "Hoodle"]
        on buttonHoodle #clicked runHoodle
        buttonChrome <-
          new Gtk.Button [#label := "Chrome"]
        on buttonChrome #clicked runChrome
        #packStart box buttonTerminal True True padding
        #packStart box buttonHoodle True True padding
        #packStart box buttonChrome True True padding
        traverse_ (addWorkspaceButton box) [1 .. numWorkspaces]

        buttonClose <-
          new Gtk.Button [#label := "Close"]
        on buttonClose #clicked Gtk.mainQuit
        #packStart box buttonClose True True padding

        set window [#child := box]
        #setKeepAbove window True
        #move window 0 0
        #showAll window
        Gtk.main
    case e of
      Left NoDisplay -> putStrLn "no display"
      Left NoMonitor -> putStrLn "no monitor"
      Right () -> pure ()
