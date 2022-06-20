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
import Foreign.C.Types (CLong)
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Enums as Gdk
import GI.Gtk (AttrOp ((:=)), get, new, on, set)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Enums as Gtk
import qualified Graphics.X11.Types as X11
  ( clientMessage,
    substructureNotifyMask,
  )
import qualified Graphics.X11.Xlib.Atom as X11
  ( cARDINAL,
    internAtom,
  )
import qualified Graphics.X11.Xlib.Display as X11
  ( closeDisplay,
    defaultRootWindow,
    openDisplay,
  )
import qualified Graphics.X11.Xlib.Event as X11
  ( allocaXEvent,
    flush,
    sendEvent,
    sync,
  )
import qualified Graphics.X11.Xlib.Extras as X11
  ( changeProperty32,
    currentTime,
    getWindowProperty32,
    propModeReplace,
    setClientMessageEvent,
    setClientMessageEvent',
    setEventType,
  )
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

getX11Property :: X11.Display -> String -> IO (Maybe [CLong])
getX11Property x11disp propName = do
  atom <- X11.internAtom x11disp propName True
  let rwin = X11.defaultRootWindow x11disp
  X11.getWindowProperty32 x11disp atom rwin

getNumberOfWorkspaces :: X11.Display -> IO Int
getNumberOfWorkspaces x11disp = do
  mval <- getX11Property x11disp "_NET_NUMBER_OF_DESKTOPS"
  let result = fromIntegral $ fromMaybe 1 $ join (fmap listToMaybe mval)
  pure result

getCurrentWorkspace :: X11.Display -> IO Int
getCurrentWorkspace x11disp = do
  mval <- getX11Property x11disp "_NET_CURRENT_DESKTOP"
  print mval
  let result = fromIntegral $ fromMaybe 1 $ join (fmap listToMaybe mval)
  pure result

changeWorkspace :: X11.Display -> Int -> IO ()
changeWorkspace x11disp idx = do
  cmd <- X11.internAtom x11disp "_NET_CURRENT_DESKTOP" True
  let arg = fromIntegral idx
  let root = X11.defaultRootWindow x11disp
  X11.allocaXEvent $ \e -> do
    X11.setEventType e X11.clientMessage
    X11.setClientMessageEvent e root cmd 32 arg X11.currentTime
    X11.sendEvent x11disp root False X11.substructureNotifyMask e
    X11.sync x11disp False

addWorkspaceButton :: X11.Display -> Gtk.Box -> Int -> IO ()
addWorkspaceButton x11disp box n = do
  button <- new Gtk.Button [#label := T.pack (show n)]
  on button #clicked $ do
    putStrLn $ "change workspace to " ++ show (n - 1)
    changeWorkspace x11disp (n - 1)
    val <- getCurrentWorkspace x11disp
    print val
  #packStart box button False False padding

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
        traverse_ (addWorkspaceButton x11disp box) [1 .. numWorkspaces]

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
