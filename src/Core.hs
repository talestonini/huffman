module Core
( Tree
, Occur
, freqTree
, prettyPrintFreqTree
, codeMap
, buildCodeMap
, prettyPrintCodeMap
, charCode
, estimateCompaction
, encodeToScreen
, encodeToFile
, decode
) where


import Control.Monad (foldM, unless, foldM_, when)
import qualified Data.Binary as B
import Data.Binary.Get (runGet, getInt64le, getRemainingLazyByteString)
import Data.Binary.Put (execPut)
import Data.Bits (Bits(shiftR))
import Data.ByteString.Builder (int64LE, hPutBuilder, word8)
import Data.ByteString.Internal (w2c)
import qualified Data.ByteString.Lazy as BL
import Data.Function (on)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.IO (withBinaryFile, IOMode(WriteMode), Handle)


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord, Generic)
instance (B.Binary a) => B.Binary (Tree a)


type Content = String
type Occur   = (String, Int)
type Code    = String
type CodeMap = Map.Map Char Code
type Bit     = Char


_debugEnabled :: Bool
_debugEnabled = False


_debugLog :: String -> IO ()
_debugLog str = when _debugEnabled $ putStrLn str


freqTree :: Content -> Tree Occur
freqTree content =
    let
        -- build the character frequency map
        buildFreqMap   = foldr (\c acc -> Map.insertWith (+) (List.singleton c) 1 acc) Map.empty
        -- sort it by frequency
        sortFreqMap fm = List.sortBy (compare `on` snd) (Map.toList fm)
        -- convert list of character -> frequency in to a list of tree leaves
        toLeafList     = List.map (\a -> Node a Empty Empty)
    in
        _buildFreqTree $ toLeafList $ sortFreqMap $ buildFreqMap content


_buildFreqTree :: [Tree Occur] -> Tree Occur
_buildFreqTree []         = Empty
_buildFreqTree [t]        = t
_buildFreqTree (t1:t2:ts) =
    let
        mergeTrees :: Tree Occur -> Tree Occur -> Tree Occur
        mergeTrees _t1@(Node v1 _ _) _t2@(Node v2 _ _) = Node (fst v1 ++ fst v2, snd v1 + snd v2) _t1 _t2
        mergeTrees Empty             _                 = Empty
        mergeTrees (Node _ _ _)      Empty             = Empty

        comparingNodeValue :: Tree Occur -> Tree Occur -> Ordering
        comparingNodeValue (Node v1 _ _) (Node v2 _ _) = snd v1 `compare` snd v2
        comparingNodeValue Empty         _             = LT
        comparingNodeValue (Node _ _ _)  Empty         = GT
    in
        _buildFreqTree $ List.insertBy comparingNodeValue (mergeTrees t1 t2) ts


prettyPrintFreqTree :: Tree Occur -> String
prettyPrintFreqTree ft = "Frequency Tree:\n" ++ show ft


codeMap :: Content -> CodeMap
codeMap content = buildCodeMap (freqTree content) (Map.empty, "")


buildCodeMap :: Tree Occur -> (CodeMap, Code) -> CodeMap
buildCodeMap Empty _ = Map.empty
buildCodeMap (Node n left right) (cm, code)
    -- if got to a leaf, insert the character -> code into the map
    | left == Empty && right == Empty = Map.insert (head $ fst n) code cm
    | otherwise                       =
        let
            -- traverse the left tree
            cmWithLeftTree = buildCodeMap left (cm, code ++ "0")
        in
            -- traverse the right tree
            buildCodeMap right (cmWithLeftTree, code ++ "1")


prettyPrintCodeMap :: CodeMap -> String
prettyPrintCodeMap cm =
    let
        code k      = charCode k cm
        prettyPrint = foldl (\acc k -> acc ++ show k ++ " - " ++ code k ++ "\n") "" (Map.keys cm)
    in
        "Code Map:\n" ++ prettyPrint ++ "\nEntry count: " ++ show (length cm)


charCode :: Char -> CodeMap -> Code
charCode c cm = fromMaybe "" (Map.lookup c cm)


-- just an estimate, due to:
-- - chars do not always fit into 1 byte (8 bits, like in the logic applied here)
-- - compacted files will have a header composed of the content length (in number of chars) and the frequency tree
estimateCompaction :: Content -> Double
estimateCompaction content =
    let
        ogSizeBits     = length content * 8  -- size in bits
        cm             = codeMap content
        code c         = charCode c cm
        encodedLenBits = foldr (\c acc -> acc + length (code c)) 0 content
    in
        fromIntegral encodedLenBits / fromIntegral ogSizeBits


-- buffer size in bytes (must be 8 if encoding with word8, which is the case in this code)
_bufferSize :: Int
_bufferSize = 8


encodeToScreen :: Content -> IO String
encodeToScreen content =
    let
        cm                  = codeMap content
        code c              = charCode c cm
        encodeChar buffer c = foldM (_bufferBit putStrLn) buffer (code c)
        rightPaddingFor str = if not (null str) then replicate (_bufferSize - length str) '0' else ""
    in
        do
        lastByte <- foldM encodeChar "" content
        return (reverse lastByte ++ rightPaddingFor lastByte)


_bufferBit :: (String -> IO ()) -> String -> Bit -> IO String
_bufferBit ioFn buffer bit =
    let
         doBuffer = bit:buffer
    in
        if length buffer + 1 == _bufferSize
            then do
                -- flush the buffer
                ioFn (reverse doBuffer)
                return ""
            else
                -- keep buffering
                return doBuffer


encodeToFile :: Content -> FilePath -> IO ()
encodeToFile content filePath = do
    let
        ft = freqTree content
    
    withBinaryFile filePath WriteMode $ \h -> do
        let
            contentLen      = int64LE $ fromIntegral (length content)
            encodedFreqTree = execPut (B.put ft)
        
        -- write header: content length (because the last byte is padded and we
        --               must stop decoding at the length) and frequency tree
        hPutBuilder h (contentLen <> encodedFreqTree)
        
        -- write body: encoded content
        lastByte <- _encodeBody content ft h
        unless (null lastByte) $
            hPutBuilder h (word8 $ _bitStringToByte lastByte)


_encodeBody :: Content -> Tree Occur -> Handle -> IO String
_encodeBody content ft h =
    let
        cm                  = buildCodeMap ft (Map.empty, "")
        code c              = charCode c cm
        _flush buffer       = hPutBuilder h (word8 $ _bitStringToByte buffer)
        encodeChar buffer c = foldM (_bufferBit _flush) buffer (code c)
        rightPaddingFor str = if not (null str) then replicate (_bufferSize - length str) '0' else ""
    in
        do
        lastByte <- foldM encodeChar "" content
        return (reverse lastByte ++ rightPaddingFor lastByte)


-- can only use this if the buffer size is 8 (due to the encoding function word8)
_bitStringToByte :: String -> B.Word8
_bitStringToByte = head . _bitStringToBytes


-- the bit string must have a length that is a multiple of 8
_bitStringToBytes :: String -> [B.Word8]
_bitStringToBytes ""   = []
_bitStringToBytes bits =
    let
        bitsWithIdx = zip bits [0..]

        bitWeight :: (Bit, Int) -> B.Word8
        bitWeight (bit, idx)
            | bit == '0' = 0x00
            | idx <= 0   = 0x80
            | otherwise  = shiftR 0x80 idx

        bitWeights = foldl (\acc b -> acc ++ [bitWeight b]) [] bitsWithIdx
    in  
        sum (take 8 bitWeights) : _bitStringToBytes (drop 8 bits)


decode :: FilePath -> IO ()
decode filePath = do
    bytes <- BL.readFile (filePath ++ "-compact")
    let
        outFile = filePath ++ "-inflated"

        (len, ft, binaryContent) = runGet (do
            _len           <- getInt64le                  -- content lenght
            _ft            <- B.get                       -- frequency tree
            _binaryContent <- getRemainingLazyByteString  -- compacted content
            return (fromIntegral _len, _ft, _binaryContent)
            ) bytes

        theEnd = (Empty, len)

        traverseTree :: (Tree Occur, Int) -> Bit -> IO (Tree Occur, Int)
        traverseTree (Node n Empty Empty, i) bit = do
            _debugLog $ "got to a leaf: char='" ++ fst n ++ "'"
            if i == len
                then return theEnd
                else do
                    appendFile outFile (fst n)
                    _debugLog $ "char count i=" ++ show (i+1)
                    traverseTree (ft, i+1) bit

        traverseTree (Node _ left _, i) '0' = do
            _debugLog "to the left..."
            return $ if i == len then theEnd else (left, i)

        traverseTree (Node _ _ right, i) '1' = do
            _debugLog "to the right..."
            return $ if i == len then theEnd else (right, i)

        traverseTree (Empty, _) _ = do
            _debugLog "the end with empty tree"
            return theEnd  -- should only be empty when we reach the padding zeroes

        traverseTree (Node _ _ _, _) _ = do
            _debugLog "unexpected end (bit is something other than 0 or 1)"
            return theEnd  -- invalid bit

        decodeByte b ftPointer outCharCount = foldM traverseTree (ftPointer, outCharCount) (_byteToBitString b)

    writeFile outFile ""
    foldM_ (\(ftPointer, outCharCount) b -> decodeByte b ftPointer outCharCount) (ft, 0) (BL.unpack binaryContent)


_byteToBitString :: B.Word8 -> String
_byteToBitString byte =
    let
        charZeroAsciiCode = 48

        decimalToBinary :: B.Word8 -> [Bit]
        decimalToBinary d
            | d == 0    = "0"
            | d == 1    = "1"
            | otherwise = w2c (d `mod` 2 + charZeroAsciiCode) : decimalToBinary (d `div` 2)

        leftPad str = replicate (8 - length str) '0' ++ str
    in
        leftPad $ reverse (decimalToBinary byte)
