module Core
( Tree
, Occur
, buildCodeMap
, freqTree
, codeMap
, prettyPrintCodeMap
, estimateCompaction
, encodeToStr
) where


import Control.Monad (foldM)
import Data.Binary (Binary)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)


type Occur = (String, Int)
data Tree a  = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Generic)
instance (Binary a) => Binary (Tree a)
type Content = String
type Code = String
type CodeMap = Map.Map Char Code
type Bit = Char


_charCode :: Char -> CodeMap -> Code
_charCode c cm = fromMaybe "" (Map.lookup c cm)


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
_buildFreqTree :: [Tree Occur] -> Tree Occur
_buildFreqTree []         = Empty
_buildFreqTree [t]        = t
_buildFreqTree (t1:t2:ts) =
    let mergeTrees         t1'@(Node v1 _ _) t2'@(Node v2 _ _) = Node (fst v1 ++ fst v2, snd v1 + snd v2) t1' t2'
        mergeTrees         Empty             _                 = Empty
        mergeTrees         (Node _ _ _)      Empty             = Empty
        comparingNodeValue (Node v1 _ _)     (Node v2 _ _)     = snd v1 `compare` snd v2
        comparingNodeValue Empty             _                 = LT
        comparingNodeValue (Node _ _ _)      Empty             = GT
    in  _buildFreqTree $ List.insertBy comparingNodeValue (mergeTrees t1 t2) ts


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
buildCodeMap Empty _ = Map.empty
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
-- - Content...: input file content
-- 
-- OUT:
-- - Tree Occur: the frequency tree
-- 
freqTree :: Content -> Tree Occur
freqTree str =
        -- build the character frequency map
    let buildFreqMap   = foldr (\ c acc -> Map.insertWith (+) (List.singleton c) 1 acc) Map.empty
        -- sort it by frequency
        sortFreqMap fm = List.sortBy (compare `on` snd) (Map.toList fm)
        -- convert list of character -> frequency in to a list of tree leaves
        toLeafList     = List.map (\ a -> Node a Empty Empty)
    in  _buildFreqTree $ toLeafList $ sortFreqMap $ buildFreqMap str


--
-- Builds the map of character (key) to code (value) from the input file.
--
-- IN:
-- - Content: input file content
-- 
-- OUT:
-- - CodeMap: the code map
-- 
codeMap :: Content -> CodeMap
codeMap content = buildCodeMap (freqTree content) (Map.empty, "")


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
    let code k     = _charCode k cm
        numEntries = "\nEntry count: " ++ show (length cm)
    in  foldl (\ acc k -> acc ++ show k ++ " - " ++ code k ++ "\n") "" (Map.keys cm) ++ numEntries


--
-- Provides an estimate rate for the compaction of the input file.
--
-- IN:
-- - Content: the input file content
--
-- OUT:
-- - Double.: the estimated compaction rate
--
estimateCompaction :: Content -> Double
estimateCompaction content =
    let ogSizeBits     = length content * 8 -- size in bits
        cm             = codeMap content
        encodedLenBits = foldr (\ c acc -> acc + length (_charCode c cm)) 0 content
    in  fromIntegral encodedLenBits / fromIntegral ogSizeBits


-- buffer size in bytes
bufferSize :: Int
bufferSize = 8


encodeToStr :: Content -> IO String
encodeToStr content =
    let cm                  = codeMap content
        encodeChar buffer c = foldM _writeBit buffer (_charCode c cm)
        padWithZeroes str   = if not (null str) then replicate (bufferSize - length str) '0' else ""
    in  do
        str <- foldM encodeChar "" content
        return (reverse str ++ padWithZeroes str)


_writeBit :: String -> Bit -> IO String
_writeBit buffer bit =
    let doBuffer = bit:buffer
    in  if length buffer + 1 == bufferSize
            then do
                -- flush the buffer
                putStrLn $ reverse doBuffer
                return ""
            else
                -- keep buffering
                return doBuffer

