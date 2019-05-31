module Main where

import SuffixTree

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
import qualified Data.Map as M


main :: IO ()
main = ui
    -- contents <- getContents
    -- print 
    --     $ reverse . sortOn (length . fst)
    --     $ M.toList
    --     $ M.filter ((>0) . length)
    --     $ M.mapWithKey (\key value -> (sort . concat) [[a,b] | a <- value, b <- value, a - b == length key]) 
    --     $ M.filterWithKey (\key _ -> length key > 1)
    --     $ findAll ""
    --     $ build "actg$" contents

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
    return ()

findAll :: String -> SuffixTree Char -> M.Map String [Int]
findAll _ (Leaf i) = M.empty
findAll cword (Node branches) = M.unionsWith (++) $ (M.fromListWith (++) (leaves)):(map (uncurry findAll) recurse)
        where leaves = map (cnt . ext) branches
              recurse = map ext branches
              ext (label, subtree) = (cword ++ label, subtree)
              cnt (label, subtree) = (label, countLeaves subtree)

countLeaves :: SuffixTree a -> [Int]
countLeaves (Leaf i) = [i]
countLeaves (Node branches) = foldl (++) [] . map (countLeaves . snd) $ branches

