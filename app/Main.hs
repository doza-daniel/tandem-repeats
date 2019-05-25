module Main where

-- import System.IO
-- import Data.List
-- import Lib

import qualified Data.Map as M


main :: IO ()
main = return ()
-- main = do
    -- contents <- getContents
    -- print $ reverse . sortOn snd . M.toList . M.filter (>5) . findAllOccurences "" $ buildSTree "actg$" contents


data SuffixTree a = Leaf Int | Node [([a], SuffixTree a)] deriving (Show)


type EdgeFunction a = [([a], Int)] -> ([a], [([a], Int)])

lazyTree edge alphabet text = let suf = suffixes text in sTree $ zip suf [0..(length suf) - 1]
      where sTree [(l, i)] = Leaf i
            sTree ss = Node [(a:p, sTree (y:ys)) | a <- alphabet, (p, y:ys) <- [edge $ begginingWith a ss]]

edge_pst :: EdgeFunction a
edge_pst [(lab, ind)] = (lab, [(lab, ind)])
edge_pst ss = ([], ss)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s@(x:xs) = s:suffixes xs

begginingWith :: (Eq a) => a -> [([a], Int)] -> [([a], Int)]
begginingWith a ss = [(ys, i) | (y:ys, i) <- ss, y == a]

build :: String -> String -> SuffixTree Char
build = lazyTree edge_pst

countLeaves :: SuffixTree a -> (Int, [Int])
countLeaves (Leaf i) = (1, [i])
countLeaves (Node branches) = foldl (\(sum, starts) (i, beg) -> (sum + i, beg++starts)) (0, []) . map (countLeaves . snd) $ branches

findAll :: String -> SuffixTree Char -> M.Map String (Int, [Int])
findAll _ (Leaf i) = M.empty
findAll cword (Node branches) = M.unionsWith merge $ (M.fromListWith merge (parovi)):(map (uncurry findAll) (map ext branches))
        where parovi = map (cnt . ext) branches
              merge (a, b) (c, d) = (a + c, b ++ d)
              ext (label, subtree) = (cword ++ label, subtree)
              cnt (label, subtree) = (label, countLeaves subtree)

--parovi cword (Node branches) = map (cnt . ext) branches
--      where ext (label, subtree) = (cword ++ label, subtree)
--            cnt (label, subtree) = (label, countLeaves subtree)
--
--merge (a, b) (c, d) = (a + c, b ++ d)


pretty' _ (Leaf i) = " -> Leaf " ++ show i
pretty' indent (Node branches) = foldl toStr "" branches
    where toStr acc (label, subtree) = acc ++ "\n" ++ (replicate indent '-') ++ show label ++ pretty' (indent + 2) subtree
