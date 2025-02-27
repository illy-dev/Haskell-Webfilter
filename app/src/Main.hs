module Main where

import Util.Wordfilter (findWords, readFiles, baseDir)
import Util.Webscraper (fetchContent, printContent, sanitizeText, WebContent(..))
import System.Directory (listDirectory)
import Data.Char (toLower)

main :: IO ()
main = do
    let url = "https://en.wikipedia.org/wiki/"
    result <- fetchContent url
    case result of
        Just content -> do
            printContent content
            
            let allContent = map (map toLower) (headings content ++ paragraphs content)
            files <- listDirectory baseDir
            swearwords <- readFiles files

            let foundWords = findWords allContent swearwords
            putStrLn $ "\nFound " ++ show (length foundWords) ++ " swear words:"
            mapM_ putStrLn foundWords
        Nothing -> putStrLn "Failed to retrieve or parse the content."