--
-- Huffman Algo: Compacts text by encoding its characters with codes whose lenght is inversely proportional to their
-- frequencies in the text.  That way, more frequent characters will have small codes, whereas less frequent characters
-- will have longer codes.  In the end, the encoded text should be shorter in storage than the original text.
--


module Main where

import Core
import qualified Data.Binary as B (encode, decode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import System.Directory.Internal.Prelude (getArgs)
import Text.Printf (printf)


commands :: [(String, FilePath -> IO ())]
commands =  [ ("encode", encodeCmd)
            , ("encodeToStr", encodeToStrCmd)
            , ("decode", decodeCmd)
            , ("estimate", estimateCmd)
            , ("printFreqTree", printFreqTreeCmd)
            , ("printCodeMap", printCodeMapCmd)
            , ("saveFreqTree", saveFreqTreeCmd)
            , ("loadFreqTree", loadFreqTreeCmd)
            ]


usage :: String
usage =
    let sep acc = if null acc then "" else ", "
        allCmds = foldl (\ acc cmdEntry -> acc ++ sep acc ++ fst cmdEntry) "" commands
    in  "Usage: ./huffman <command> <filePath>\n\nwhere <command> is one of: " ++ allCmds


main :: IO ()
main = do
    args <- getArgs
    case args of
        [cmd, filePath] -> case lookup cmd commands of
            Just c  -> c filePath
            Nothing -> putStrLn ("Invalid command: " ++ cmd ++ "\n\n" ++ usage)
        _               -> putStrLn ("Invalid arguments.\n\n" ++ usage)


encodeCmd :: FilePath -> IO ()
encodeCmd filePath = do
    content <- readFile filePath
    encode content (filePath ++ "-compact")


encodeToStrCmd :: FilePath -> IO ()
encodeToStrCmd filePath = do
    content <- readFile filePath
    str     <- encodeToStr content
    putStr str


decodeCmd :: FilePath -> IO ()
decodeCmd = undefined


estimateCmd :: FilePath -> IO ()
estimateCmd filePath = do
    content <- readFile filePath
    printf "Estimated compaction rate: %.3f\n" (estimateCompaction content)


printFreqTreeCmd :: FilePath -> IO ()
printFreqTreeCmd filePath = do
    content <- readFile filePath
    print (freqTree content)


printCodeMapCmd :: FilePath -> IO ()
printCodeMapCmd filePath = do
    content <- readFile filePath
    putStrLn $ prettyPrintCodeMap $ codeMap content


saveFreqTreeCmd :: FilePath -> IO ()
saveFreqTreeCmd filePath = do
    content <- readFile filePath
    let fullFilePath = filePath ++ "-compact"
    BL.writeFile fullFilePath (B.encode $ freqTree content)


loadFreqTreeCmd :: FilePath -> IO ()
loadFreqTreeCmd filePath = do
    let fullFilePath = filePath ++ "-compact"
    binaryContent <- BL.readFile fullFilePath
    let ft = B.decode binaryContent :: Tree Occur
        cm = buildCodeMap ft (Map.empty, "")
    print ft
    putStrLn ""
    putStrLn (prettyPrintCodeMap cm)
