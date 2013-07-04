module Main where

import Data.Char
import Data.List
import Data.Tree

main :: IO ()
main = do
  input <- getContents
  putStrLn $ format input

format :: String -> String
format = tabify False 0 .
         unwords .
         words
  where
    tabify _ _ []  = []
    tabify _ _ [c] = [c]
    tabify nl n (c:str)
      | c `elem` opens = c : tabify True (n+1) str
      | c `elem` delim = c : tabify True n str
      | c `elem` close = c : tabify False (n-1) str
      | nl             = condTabs nl n ++ skipSpaces False n (c:str)
      | otherwise      = c : tabify nl n str

    skipSpaces nl n str = case dropWhile isSpace str of
      (c:cs) -> c : tabify nl n cs
      [] -> []

    condTabs nl n
      | nl = tabs n
      | otherwise = ""

    tabs n = '\n' : replicate (2*n) ' '

    opens = "<{(["
    close = ">})]"
    delim = ",;"

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