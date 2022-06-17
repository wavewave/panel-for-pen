{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gdk.Enums as Gdk
import GI.Gtk (AttrOp ((:=)), new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk

main :: IO ()
main = do
  Gtk.init Nothing
  window <-
    new
      Gtk.Window
      [ #title := "panel-for-pen",
        #gravity := Gdk.GravityNorthWest,
        #decorated := False
      ]
  on window #destroy Gtk.mainQuit
  button <- new Gtk.Button [#label := "Close"]
  on button #clicked Gtk.mainQuit
  set window [#child := button]
  #setKeepAbove window True
  #move window 0 0
  #showAll window
  Gtk.main
