{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Error.Util (failWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Gdk
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk

data PanelError = NoDisplay | NoMonitor
  deriving (Show)

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
      Gtk.widgetSetSizeRequest window width 30
      on window #destroy Gtk.mainQuit
      button <- new Gtk.Button [#label := "Close"]
      on button #clicked Gtk.mainQuit
      set window [#child := button]
      #setKeepAbove window True
      #move window 0 0
      #showAll window
      Gtk.main
  case e of
    Left NoDisplay -> putStrLn "no display"
    Left NoMonitor -> putStrLn "no monitor"
    Right () -> pure ()
