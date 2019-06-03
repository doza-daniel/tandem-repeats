module Main where


import TandemRepeats

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)


main :: IO ()
main = ui

ui :: IO ()
ui = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "ui.glade"
    mainWindow <- builderGetObject builder castToWindow "main_window"
    mainWindow `on` deleteEvent $ do
        liftIO mainQuit
        return False

    sequenceInput <- builderGetObject builder castToTextView "sequence_input"
    runBtn <- builderGetObject builder castToButton "run_button"
    runBtn `on` buttonActivated $ do run sequenceInput
    widgetShowAll mainWindow
    mainGUI

run :: TextView -> IO ()
run tview = do
    tbuff <- textViewGetBuffer tview
    start <- textBufferGetStartIter tbuff
    end <- textBufferGetEndIter tbuff
    seqInput <- (textBufferGetText tbuff start end True) :: IO String
    textBufferSetText tbuff $ let s =  parse seqInput in
                                case s of Nothing -> "Parse error"
                                          Just parsed -> format $ solve parsed
