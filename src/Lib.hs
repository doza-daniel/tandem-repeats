module Lib
    ( buildSTree
    , findAllOccurences
    , pretty
    ) where

import Data.Char
import qualified Data.Map as M

data SuffixTree a = Leaf | Node { branches :: [([a], SuffixTree a)] } deriving (Show)
type EdgeFunction a = [[a]] -> ([a], [[a]])

lazyTree :: (EdgeFunction Char) -> String -> String -> SuffixTree Char
lazyTree edge alphabet text = sTree (suffixes text)
    where sTree [[]] = Leaf
          sTree ss = Node [(a:p, sTree (y:ys)) | a <- alphabet, (p, y:ys) <- [edge $ begginingWith a ss]]

begginingWith :: Char -> [String] -> [String]
begginingWith a ss = [ys | y:ys <- ss, y == a]

buildSTree ::  String -> String -> SuffixTree Char
buildSTree = lazyTree edge_pst

edge_pst :: EdgeFunction Char
edge_pst [s] = (s, [[]])
edge_pst ss = ([], ss)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s@(x:xs) = s:suffixes xs

pretty _ Leaf = " -> Leaf"
pretty indent (Node branches) = foldl toStr "" branches
    where toStr acc (label, subtree) = acc ++ "\n" ++ (replicate indent '-') ++ show label ++ pretty (indent + 2) subtree

cntL :: SuffixTree a -> Int
cntL Leaf = 1
cntL node = sum $ map (cntL . snd) $ branches node

findAllOccurences :: String -> SuffixTree Char -> M.Map String Int
findAllOccurences _ Leaf = M.empty
findAllOccurences cword (Node b) = M.unionsWith (+) $ (M.fromListWith (+) parovi):(map (uncurry findAllOccurences) (map (\(lab, st) -> (cword ++ lab, st)) b)) where parovi = map (\(label, subtree) -> let currWord = cword ++ label in (currWord, cntL subtree)) b


