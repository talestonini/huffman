-- Huffman Algo: Compacts text by encoding its characters with codes whose lenght is inversally proportional to their
-- frequencies in the text.  That way, more frequent characters will have small codes, whereas less frequent characters
-- will have longer codes.  In the end, the encoded text should be shorter in storage than the original text.
--
-- 0. Obtain text input.
-- 1. Build map of character frequencies.
-- 2. Sort it by frequencies.
-- 3. Build the tree of character codes.
-- 4. Traverse input text, replacing each character by their code.
--

import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map

type FreqMap = Map.Map String Int
type Occur = (String, Int)
data Tree a = Empty | Leaf { val :: a } | Node { val :: a, left :: Tree a, right :: Tree a } deriving (Show, Eq, Ord)

-- Note that each char in the input string is converted to a single-char string in the output map.  This will help
-- build the tree of frequencies.
buildFreqMap :: String -> FreqMap
buildFreqMap = foldr (\ c acc -> Map.insertWith (+) (List.singleton c) 1 acc) Map.empty

sortFreqMap :: FreqMap -> [Occur]
sortFreqMap fm = List.sortBy (compare `on` snd) (Map.toList fm)

toLeafList :: (Show a, Eq a, Ord a) => [a] -> [Tree a]
toLeafList = List.map Leaf

-- Traverses list of leaves to build the tree.
buildFreqTree :: [Tree Occur] -> Tree Occur
buildFreqTree [t]        = t
buildFreqTree (t1:t2:ts) =
  let compareValueFreq t1 t2 = snd (val t1) `compare` snd (val t2)
      mergeTrees t1 t2 = Node (fst (val t1) ++ fst (val t2), snd (val t1) + snd (val t2)) t1 t2
  in buildFreqTree $ List.insertBy compareValueFreq (mergeTrees t1 t2) ts

chain :: String -> Tree Occur
chain str = buildFreqTree $ toLeafList $ sortFreqMap $ buildFreqMap str