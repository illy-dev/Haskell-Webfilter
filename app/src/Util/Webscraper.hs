{-# LANGUAGE OverloadedStrings #-}

module Util.Webscraper (fetchContent, printContent, scrapeContent, sanitizeText, WebContent(..)) where

import Text.HTML.Scalpel
import Data.Text (Text, pack, unpack, replace)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Char (isAlphaNum)

data WebContent = WebContent
    { headings :: [String]
    , paragraphs :: [String]
    } deriving (Show)

scrapeContent :: Scraper String WebContent
scrapeContent = do
    h1s <- texts "h1"
    h2s <- texts "h2"
    h3s <- texts "h3"
    h4s <- texts "h4"
    h5s <- texts "h5"
    h6s <- texts "h6"
    ps <- texts "p"
    let hs = concat [h1s, h2s, h3s, h4s, h5s, h6s]
    return $ WebContent hs ps

fetchContent :: String -> IO (Maybe WebContent)
fetchContent url = scrapeURL url scrapeContent

sanitizeText :: String -> String
sanitizeText = filter (\c -> isAlphaNum c || c == ' ')

printContent :: WebContent -> IO ()
printContent (WebContent hs ps) = do
    mapM_ (TIO.putStrLn . pack . sanitizeText) hs
    mapM_ (TIO.putStrLn . pack . sanitizeText) ps

main :: IO ()
main = do
    let url = "https://en.wikipedia.org/wiki/haskell"
    result <- fetchContent url
    case result of
        Just content -> printContent content
        Nothing -> putStrLn "Failed to retrieve or parse the content."