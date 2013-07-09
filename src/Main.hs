module Main where

import Data.Tree
import Data.List

import Hunf.Format

main :: IO ()
main = do
  input <- getContents
  putStrLn $ format input

-- | Demonstration

ppTree :: Show a => Tree a -> String
ppTree (Node a ch) = "(" ++ show a ++ "," ++ "[" ++ intercalate "," (map ppTree ch) ++ "]" ++ ")"

mkTree :: Tree Int
mkTree = unfoldTree f 0
  where
    f n | n <= 4    = (n,[n+1..n+3])
        | otherwise = (n,[])

test :: IO ()
test = do
  let underlineLn str = do
        putStrLn str
        putStrLn (replicate (length str) '=')
  
  underlineLn "Derived show"
  putStrLn $ format $ show mkTree
  underlineLn "Prettyprint"
  putStrLn $ format $ ppTree mkTree