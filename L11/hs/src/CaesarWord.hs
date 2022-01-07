module CaesarWord where

import System.Environment (getArgs)
import qualified Data.Char as Char

shift :: Int -> Char -> Char
shift d c
  | Char.isAlpha c = tr c
  | otherwise = c where
      tr c = (Char.chr . sh . Char.ord . Char.toLower) c
      sh c = (c - shA + d) `mod` 26 + shA
      shA = Char.ord 'a'

decrypt :: Int -> String -> String
decrypt p c = map (shift (26 - p)) c

streq :: String -> String -> Bool -- check if a string contains the b string in the initial position
streq _ ""  = True
streq "" _  = False
streq sa sb = 
    case sa of
        (x:xs) -> 
            case sb of 
                (y:ys) -> if (x /= y) then False else streq xs ys

strstr :: String -> String -> Bool --check if a substring exists in a string
strstr "" _ = False
strstr _ "" = False
strstr s sub = if (s `streq` sub) then True else case s of (_:xs) -> strstr xs sub  


crackCypher :: String -> String -> Maybe Int
crackCypher "" _ = Nothing
crackCypher _ "" = Nothing
crackCypher cypher known = 
    let 
        bruteForce key = 
            if key == 26 then 
                Nothing 
            else 
                if strstr (decrypt key cypher) known then 
                    Just key
                else 
                    bruteForce (key+1) 
    in
        bruteForce 1

main :: IO ()
main = 
    do
        text <- getLine
        args <- getArgs
        let word = head args
        putStrLn "Hello World"
        