{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Gdk
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk

main :: IO ()
main = do
  Gtk.init Nothing
  mdisplay <- Gdk.displayGetDefault
  case mdisplay of
    Nothing -> error "no display?"
    Just display -> do
      mmonitor <- #getPrimaryMonitor display
      case mmonitor of
        Nothing -> error "no monitor?"
        Just monitor -> do
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
