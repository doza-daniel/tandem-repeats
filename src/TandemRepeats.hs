module TandemRepeats
    ( solve
    , format
    , parse
    ) where

import SuffixTree
import Data.Char
import Data.List
import qualified Data.Map as M

parse :: String -> Maybe String
parse input = if not (all isBase clean) then Nothing else Just (clean ++ "$")
                    where clean = filter isAlpha $  unlines $ filter (not . (=='>') . head) $  filter (not . null) $ lines input

isBase c
    | c == 'A' = True
    | c == 'T' = True
    | c == 'C' = True
    | c == 'G' = True
    | otherwise = False

solve input = reverse . sortOn (length . fst)
            $ M.toList
            $ M.filter ((>0) . length)
            $ M.mapWithKey (\key value ->  (filter ((>1).length).grp (length key).sort) value) 
            $ M.filterWithKey (\key _ -> length key > 1)
            $ findAll ""
            $ build "ACTG$" input

format input = unlines ["Label: " ++ label ++ "\nIndices: " ++ show indices | (label, indices) <- input]

grp _ [] = []
grp diff (x:xs) =  hd : (grp diff [y | y <- xs, not $ y `elem` hd])
    where hd = foldl (\acc a -> if (abs (a - head acc)) == diff then a:acc else acc) [x] xs

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

