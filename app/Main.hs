{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Error.Util (failWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Word (Word32)
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Gdk
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk
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

main :: IO ()
main = do
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
      buttonClose <-
        new Gtk.Button [#label := "Close"]
      on buttonClose #clicked Gtk.mainQuit
      #packStart box buttonTerminal True True padding
      #packStart box buttonHoodle True True padding
      #packStart box buttonChrome True True padding
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
