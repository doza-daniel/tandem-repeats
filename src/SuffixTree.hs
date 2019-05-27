module SuffixTree
    ( build
    , SuffixTree(..)
    , pretty
    ) where

import Data.Char
import qualified Data.Map as M

data SuffixTree a = Leaf Int | Node [([a], SuffixTree a)] deriving (Show)
type EdgeFunction a = [([a], Int)] -> ([a], [([a], Int)])

lazyTree edge alphabet text = let suf = suffixes text in sTree $ zip suf [0..(length suf) - 1]
      where sTree [(l, i)] = Leaf i
            sTree ss = Node [
                (a:label, sTree (y:ys)) |
                a <- alphabet,
                (label, y:ys) <- [edge $ begginingWith a ss]]
            begginingWith a ss = [(ys, i) | (y:ys, i) <- ss, y == a]

edge_pst :: EdgeFunction a
edge_pst [(lab, ind)] = (lab, [(lab, ind)])
edge_pst ss = ([], ss)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s@(x:xs) = s:suffixes xs

build :: String -> String -> SuffixTree Char
build = lazyTree edge_pst


pretty _ (Leaf i) = " -> Leaf " ++ show i
pretty indent (Node branches) = foldl toStr "" branches
    where toStr acc (label, subtree) = acc
                                        ++ "\n"
                                        ++ (replicate indent '-')
                                        ++ show label
                                        ++ pretty (indent + 2) subtree
