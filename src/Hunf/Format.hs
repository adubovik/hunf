module Hunf.Format(format,showF) where

import Data.Char

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
      | nl             = condTabs nl n ++ tabify False n (dropWhile isSpace $ c:str)
      | otherwise      = c : tabify nl n str

    condTabs nl n
      | nl = tabs n
      | otherwise = ""

    tabs n = '\n' : replicate (2*n) ' '

    opens = "<{(["
    close = ">})]"
    delim = ",;"

showF :: Show a => a -> String
showF = format . show