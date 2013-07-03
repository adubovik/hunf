module Main where

import Data.List
import Data.Tree

main :: IO ()
main = do
  input <- getContents
  putStrLn $ format input

ppTree :: Show a => Tree a -> String
ppTree (Node a ch) = "(" ++ show a ++ "," ++ "[" ++ intercalate "," (map ppTree ch) ++ "]" ++ ")"

mkTree :: Tree Int
mkTree = unfoldTree f 0
  where
    f n | n <= 4    = (n,[n+1..n+3])
        | otherwise = (n,[])

format :: String -> String
format = tabify 0 .
         unwords .
         words
  where
    tabify _ [] = []
    tabify _ [c] = [c]
    tabify n (c:c':str)
      | c `elem` opens && c' `elem` opens  = c : tabify (n+1) (c':str)
    tabify n (c:str)
      | c `elem` opens = c : (tabs (n+1)) ++ tabify (n+1) str
      | c `elem` delim = c : (tabs n) ++ tabify n str
      | c `elem` close = c : tabify (n-1) str
      | otherwise      = c : tabify n str

    tabs n = '\n' : replicate (2*n) ' '

    opens = "<{(["
    close = ">})]"
    delim = ",;"

test :: IO ()
test = do
  putStrLn "Records"
  putStrLn $ format $ show mkTree
  putStrLn "Prettyprint"
  putStrLn $ format $ ppTree mkTree