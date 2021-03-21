module Main where

import System.IO
import Text.Printf
import Data.List
import Data.Char
import qualified Data.HashMap.Strict as M

count :: [String] -> M.HashMap String Int -> M.HashMap String Int
count xs m = foldl' f m xs
    where f m x = M.insertWith (+) x 1 m

countWords :: String -> M.HashMap String Int
countWords line = count w M.empty
    where w = words line

printCount :: [(String, Int)] -> IO ()
printCount [] = putStrLn ""
printCount ((a,b):xs) = do
    putStrLn (printf "%s %d" a b)
    printCount xs

main :: IO ()
main = do
    hSetBuffering stdin (BlockBuffering (Just (1024 * 64)))
    contents <- getContents
    let lowered = map toLower contents
        counted = countWords lowered
        sorted = sortBy (\ (_,a) (_,b) -> compare b a) $ M.toList counted
    printCount sorted

