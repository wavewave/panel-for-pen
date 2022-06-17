{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk

main :: IO ()
main = do
  Gtk.init Nothing
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.onWidgetDestroy window Gtk.mainQuit
  Gtk.setWindowTitle window "Hello World"
  Gtk.widgetShowAll window
  Gtk.main
