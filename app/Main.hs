module Main where

import System.IO
import Data.List
import Lib

import qualified Data.Map as M


main :: IO ()
main = do
    contents <- getContents
    print $ reverse . sortOn snd . M.toList . M.filter (>5) . findAllOccurences "" $ buildSTree "actg$" contents
