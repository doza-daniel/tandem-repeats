module Main where

import SuffixTree

import Data.List
import qualified Data.Map as M


main :: IO ()
main = do
    contents <- getContents
    print 
        $ reverse . sortOn (length . fst)
        $ M.toList
        $ M.filter ((>0) . length)
        $ M.mapWithKey (\key value -> (sort . concat) [[a,b] | a <- value, b <- value, a - b == length key]) 
        $ M.filterWithKey (\key _ -> length key > 1)
        $ findAll ""
        $ build "actg$" contents

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

