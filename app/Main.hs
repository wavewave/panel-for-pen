{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GI.Gtk (AttrOp ((:=)), new, on)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk

main :: IO ()
main = do
  Gtk.init Nothing
  window <- new Gtk.Window [#title := "Hello World"]
  on window #destroy Gtk.mainQuit
  #showAll window
  Gtk.main
