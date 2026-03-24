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

type FreqMap = Map.Map Char Int
type Occur = (Char, Int)
data Tree a = Leaf | Node a

buildFreqMap :: String -> FreqMap
buildFreqMap = foldr (\ c acc -> Map.insertWith (+) c 1 acc) Map.empty

sortFreqMap :: FreqMap -> [Occur]
sortFreqMap fm = List.sortBy (compare `on` snd) (Map.toList fm)