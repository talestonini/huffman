module Core
( Tree
, Occur
, buildCodeMap
, freqTree
, codeMap
, prettyPrintCodeMap
, estimateCompaction
) where


import Data.Binary (Binary)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics as G


type Occur = (String, Int)
data Tree a  = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Generic)
instance (Binary a) => Binary (Tree a)
type Code = String
type CodeMap = Map.Map Char Code


--
-- Builds the frequency tree by traversing the list of leaves.  A leaf has a distinct character from the input file and
-- its corresponding frequency (occurrence count) in the file.
--
-- IN:
-- - [Tree Occur]: the list of tree leaves
--
-- OUT:
-- - Tree Occur..: the frequency tree
--
buildFreqTree :: [Tree Occur] -> Tree Occur
buildFreqTree [t]        = t
buildFreqTree (t1:t2:ts) =
    let mergeTrees         t1@(Node v1 _ _) t2@(Node v2 _ _) = Node (fst v1 ++ fst v2, snd v1 + snd v2) t1 t2
        comparingNodeValue t1@(Node v1 _ _) t2@(Node v2 _ _) = snd v1 `compare` snd v2
    in  buildFreqTree $ List.insertBy comparingNodeValue (mergeTrees t1 t2) ts


-- 
-- Builds the map of character (key) to code (value).  The character is a distinct character from the input file and
-- their code is built by traversing the frequency tree: build the code by adding a "0" bit when navigating to the left
-- and a "1" bit when navigating to the right.
-- 
-- IN:
-- - Tree Occur.....: the frequency tree
-- - (CodeMap, Code): accumulators for the map and the code
-- 
-- OUT:
-- - CodeMap........: the final value of the map accumulator
-- 
buildCodeMap :: Tree Occur -> (CodeMap, Code) -> CodeMap
buildCodeMap (Node v left right) (cm, code)
    -- if got to a leaf, insert the character -> code into the map
    | left == Empty && right == Empty = Map.insert (head $ fst v) code cm
    | otherwise =
            -- traverse the left tree
        let cmWithLeftTree = buildCodeMap left (cm, code ++ "0")
            -- traverse the right tree
        in  buildCodeMap right (cmWithLeftTree, code ++ "1")


--
-- Builds the frequency tree from the input file.  Note that each distinct character in the input string is converted to
-- a single-character string in the output map.
-- 
-- IN:
-- - String....: input file content
-- 
-- OUT:
-- - Tree Occur: the frequency tree
-- 
freqTree :: String -> Tree Occur
freqTree str =
        -- build the character frequency map
    let buildFreqMap   = foldr (\ c acc -> Map.insertWith (+) (List.singleton c) 1 acc) Map.empty
        -- sort it by frequency
        sortFreqMap fm = List.sortBy (compare `on` snd) (Map.toList fm)
        -- convert list of character -> frequency in to a list of tree leaves
        toLeafList     = List.map (\ a -> Node a Empty Empty)
    in  buildFreqTree $ toLeafList $ sortFreqMap $ buildFreqMap str


--
-- Builds the map of character (key) to code (value) from the input file.
--
-- IN:
-- - String.: input file content
-- 
-- OUT:
-- - CodeMap: the code map
-- 
codeMap :: String -> CodeMap
codeMap str = buildCodeMap (freqTree str) (Map.empty, "")


--
-- Prints a human-readable map of the code map.
--
-- IN:
-- - CodeMap: the code map
-- 
-- OUT:
-- - String.: a human-readable map of the code map
-- 
prettyPrintCodeMap :: CodeMap -> String
prettyPrintCodeMap cm =
    let code k     = fromMaybe "" (Map.lookup k cm)
        numEntries = "\nEntry count: " ++ show (length cm)
    in  foldl (\ acc k -> acc ++ show k ++ " - " ++ code k ++ "\n") "" (Map.keys cm) ++ numEntries


--
-- Provides an estimate rate for the compaction of the input file.
--
-- IN:
-- - String: the input file content
--
-- OUT:
-- - Double: the estimated compaction rate
--
estimateCompaction :: String -> Double
estimateCompaction str =
    let ogSizeBits     = length str * 8 -- size in bits
        cm             = codeMap str
        code c         = fromMaybe "" (Map.lookup c cm)
        encodedLenBits = foldr (\ c acc -> acc + length (code c)) 0 str
    in  fromIntegral encodedLenBits / fromIntegral ogSizeBits
