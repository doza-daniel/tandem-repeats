module Main where

import Data.Char
import Lib

main :: IO ()
main = someFunc

data SuffixTree a = Leaf | Branch [([a], SuffixTree a)]
type EdgeFunction a = [[a]] -> ([a], [[a]])

lazyTree :: (Eq a) => (EdgeFunction a) -> [a] -> [a] -> SuffixTree a
lazyTree edge alphabet text = sTree (suffixes text)
    where sTree [[]] = Leaf
          sTree ss = Branch [(a:p, sTree (y:ys)) |  a <- alphabet, (p, y:ys) <- [edge $ begginingWith a ss]]


begginingWith a ss = [ys | y:ys <- ss, y == a]

lazy_pst :: (Eq a) => [a] -> [a] -> SuffixTree a
lazy_pst = lazyTree edge_pst

edge_pst :: EdgeFunction a
edge_pst [s] = (s, [[]])
edge_pst ss = ([], ss)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s@(x:xs) = s:suffixes xs



instance (Show a) => Show (SuffixTree a) where
    show = dropWhile isControl . pretty' 0

pretty' _ Leaf = " -> Leaf"
pretty' indent (Branch nodes) = foldl toStr "" nodes
    where toStr acc (label, subtree) = acc ++ "\n" ++ (replicate indent '-') ++ show label ++ pretty' (indent + 2) subtree




