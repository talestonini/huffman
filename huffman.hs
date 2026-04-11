-- Huffman Algo: Compacts text by encoding its characters with codes whose lenght is inversely proportional to their
-- frequencies in the text.  That way, more frequent characters will have small codes, whereas less frequent characters
-- will have longer codes.  In the end, the encoded text should be shorter in storage than the original text.


import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Directory.Internal.Prelude (getArgs)
import System.IO (readFile)
import Text.Printf (printf)


type FreqMap = Map.Map String Int
type Occur   = (String, Int)
data Tree a  = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)
type CodeMap = Map.Map Char String


commands :: [(String, FilePath -> IO ())]
commands =  [ ("encode", encodeCmd)
            , ("decode", decodeCmd)
            , ("estimate", estimateCmd)
            , ("freqTree", freqTreeCmd)
            , ("codeMap", codeMapCmd)
            ]


usage :: String
usage = "Usage: ./huffman <command> <filePath>\n\nwhere <command> is one of: encode, decode"


main = do
    args <- getArgs
    case args of
        [cmd, filePath] -> case lookup cmd commands of
            Just c  -> c filePath
            Nothing -> putStrLn $ "Invalid command: " ++ cmd ++ "\n\n" ++ usage
        _               -> putStrLn $ "Invalid arguments.\n\n" ++ usage


encodeCmd :: FilePath -> IO ()
encodeCmd = undefined


decodeCmd :: FilePath -> IO ()
decodeCmd = undefined


estimateCmd :: FilePath -> IO ()
estimateCmd filePath = do
    contents <- readFile filePath
    printf "Estimated compaction rate: %.3f\n" (estimateCompaction contents)


freqTreeCmd :: FilePath -> IO ()
freqTreeCmd filePath = do
    content <- readFile filePath
    print $ freqTree content


codeMapCmd :: FilePath -> IO ()
codeMapCmd filePath = do
    contents <- readFile filePath
    let cm         = codeMap contents
        code k     = fromMaybe "" (Map.lookup k cm)
        numEntries = "\nEntry count: " ++ show (length cm)
    putStrLn $ foldl (\ acc k -> acc ++ show k ++ " - " ++ code k ++ "\n") "" (Map.keys cm) ++ numEntries


------------------------------------------------------------------------------------------------------------------------


-- Note that each distinct char in the input string is converted to a single-char string in the output map.  This will
-- help build the tree of frequencies.
buildFreqMap :: String -> FreqMap
buildFreqMap = foldr (\ c acc -> Map.insertWith (+) (List.singleton c) 1 acc) Map.empty


sortFreqMap :: FreqMap -> [Occur]
sortFreqMap fm = List.sortBy (compare `on` snd) (Map.toList fm)


toLeafList :: (Show a, Eq a, Ord a) => [a] -> [Tree a]
toLeafList = List.map (\ a -> Node a Empty Empty)


-- Traverses list of leaves to build the tree.
buildFreqTree :: [Tree Occur] -> Tree Occur
buildFreqTree [t]        = t
buildFreqTree (t1:t2:ts) =
    let mergeTrees         t1@(Node v1 _ _) t2@(Node v2 _ _) = Node (fst v1 ++ fst v2, snd v1 + snd v2) t1 t2
        comparingValueFreq t1@(Node v1 _ _) t2@(Node v2 _ _) = snd v1 `compare` snd v2
    in buildFreqTree $ List.insertBy comparingValueFreq (mergeTrees t1 t2) ts


buildCodeMap :: Tree Occur -> (CodeMap, String) -> CodeMap
buildCodeMap (Node v left right) (cm, code)
    | left == Empty && right == Empty = Map.insert (head $ fst v) code cm
    | otherwise =
        let cmWithLeftTree = buildCodeMap left (cm, code ++ "0")
        in buildCodeMap right (cmWithLeftTree, code ++ "1")


freqTree :: String -> Tree Occur
freqTree str = buildFreqTree $ toLeafList $ sortFreqMap $ buildFreqMap str


codeMap :: String -> CodeMap
codeMap str = buildCodeMap (freqTree str) (Map.empty, "")


estimateCompaction :: String -> Double
estimateCompaction str =
    let ogSizeBits     = length str * 8 -- size in bits
        cm             = codeMap str
        code c         = fromMaybe "" (Map.lookup c cm)
        encodedLenBits = foldr (\ c acc -> acc + length (code c)) 0 str
    in fromIntegral encodedLenBits / fromIntegral ogSizeBits
