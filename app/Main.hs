--
-- Huffman Algo: Compacts text by encoding its characters with codes whose lenght is inversely proportional to their
-- frequencies in the text.  That way, more frequent characters will have smaller codes, whereas less frequent
-- characters will have longer codes.  In the end, the encoded text should be shorter in storage than the original text.
--


module Main where

import Core
import qualified Data.Binary as B
import Data.Binary.Get (runGet, getInt64le)
import Data.Binary.Put (runPut, putInt64le)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf)


commands :: [(String, FilePath -> IO ())]
commands =  [ ("printFreqTree", printFreqTreeCmd)
            , ("printCodeMap", printCodeMapCmd)
            , ("estimate", estimateCmd)
            , ("saveHeader", saveHeaderCmd)
            , ("loadHeader", loadHeaderCmd)
            , ("encodeToScreen", encodeToScreenCmd)
            , ("encode", encodeCmd)
            , ("decode", decodeCmd)
            ]

        
compactSuffix :: String
compactSuffix = "-compact"


inflatedSuffix :: String
inflatedSuffix = "-inflated"


usage :: String
usage =
    let
        sep acc = if null acc then "" else ", "
        allCmds = foldl (\acc cmdEntry -> acc ++ sep acc ++ fst cmdEntry) "" commands
    in
        "Usage: ./huffman <command> <filePath>\n\nwhere <command> is one of: " ++ allCmds


main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd, filePath] -> case lookup cmd commands of
            Just c  -> c filePath
            Nothing -> putStrLn ("Invalid command: " ++ cmd ++ "\n\n" ++ usage)
        _               -> putStrLn ("Invalid arguments.\n\n" ++ usage)


printFreqTreeCmd :: FilePath -> IO ()
printFreqTreeCmd filePath = do
    content <- readFile filePath
    putStrLn $ prettyPrintFreqTree (freqTree content)


printCodeMapCmd :: FilePath -> IO ()
printCodeMapCmd filePath = do
    content <- readFile filePath
    putStrLn $ prettyPrintCodeMap (codeMap content)


estimateCmd :: FilePath -> IO ()
estimateCmd filePath = do
    content <- readFile filePath
    printf "Estimated compaction rate (compacted / original size): %.3f\n" (estimateCompaction content)


saveHeaderCmd :: FilePath -> IO ()
saveHeaderCmd filePath = do
    content <- readFile filePath
    let
        bytes = runPut $ do
            putInt64le $ fromIntegral (length content)
            B.put (freqTree content)

    BL.writeFile (filePath ++ compactSuffix) bytes


loadHeaderCmd :: FilePath -> IO ()
loadHeaderCmd filePath = do
    bytes <- BL.readFile (filePath ++ compactSuffix)
    let
        (len, ft) = runGet (do
            _len <- getInt64le
            _ft  <- B.get
            return (_len, _ft)
            ) bytes 

        cm = buildCodeMap ft (Map.empty, "")

    putStrLn (prettyPrintFreqTree ft)
    putStrLn ""
    putStrLn ("Characters length: " ++ show len)
    putStrLn ""
    putStrLn (prettyPrintCodeMap cm)


encodeToScreenCmd :: FilePath -> IO ()
encodeToScreenCmd filePath = do
    content <- readFile filePath
    str     <- encodeToScreen content
    putStr str


encodeCmd :: FilePath -> IO ()
encodeCmd filePath = do
    content <- readFile filePath
    encodeToFile content (filePath ++ compactSuffix)


decodeCmd :: FilePath -> IO ()
decodeCmd filePath = decode (filePath ++ compactSuffix) (filePath ++ inflatedSuffix)
