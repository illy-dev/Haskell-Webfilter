module Util.Wordfilter (findWords, readFiles, baseDir) where

import System.IO
import Data.Char (toLower)
import System.FilePath ((</>))
import System.Directory (listDirectory)

baseDir :: FilePath
baseDir = "app/resources/swearlists"

replaceChars :: String -> String
replaceChars word
    | length word <= 2 = word
    | otherwise = take 2 word ++ replicate (length word - 3) '*' ++ [last word]

elem' :: [String] -> [String] -> [String]
elem' _ [] = []
elem' [] _ = []
elem' (x:xs) ys = map replaceChars (filter (`elem` ys) (x:xs))

findWords :: [String] -> [String] -> [String]
findWords [] _ = []
findWords (x:xs) y = elem' [x] y ++ findWords xs y


readFiles :: [FilePath] -> IO [String]
readFiles paths = do
    let fullPaths = map (baseDir </>) paths
    fileContents <- mapM readFile fullPaths
    let fileLines = concatMap (map (map toLower) . lines) fileContents
    return fileLines


main :: IO ()
main = do
    files <- listDirectory baseDir
    swearwords <- readFiles files

    print $ findWords (words "test asdasda hallo kacke") swearwords