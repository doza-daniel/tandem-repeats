module Main where

import System.IO
import Lib

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ pretty 0 $ buildSTree "actg$" contents

