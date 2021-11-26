
module L08 where

{-
    This is my file

    E 8.8.1: Test in GHCi: fact 10000 and fact 100000 . What takes longer, calculating the number or printing all of its digits?
    A   : It seems like it takes longer printing all of its digits


    Q 8.13.1: Enumerate:
            - 2 differences between Elm and Haskell

                1. Haskell can import all except a type or function
                2. Haskell support list comprehentions, Elm doesn't
                    funny, since Haskell descourages its use

            - 2 features of Haskell that are not present in SML

                1. List comprehention
                2. lazy evaluation

            - 2 features of SML that are not present in Haskell

                1. it supports mutable references and exceptions
                2. semantics are formally specified and verified
-}

{-  E 8.14.1 Rewrite the sudan function from lab 1 using where in Haskell. -}

sudan :: Integral a => a -> a -> a -> a
sudan n x y 
    | n == 0 = x + y
    | y == 0 = x
    | otherwise = sudan (n - 1) sudany (y + sudany) where sudany = sudan n x (y - 1)


{-  E 8.14.2 Define an infix operator called !& in Haskell, which implements the logical function nand-}

infixl !&

True !& True = False
_ !& _ = True

{-  E 8.14.3 Implement the safeHead :: [a] -> Maybe a and safeTail :: [a] -> Maybe [a] functions
            in Haskell which return Nothing if the list is empty and the result wrapped in Just if it
            isn’t empty-}

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

{-  E 8.14.4 Write a function called average :: [Int] -> Float , which calculates the average of a list
            of integers.-}

average :: [Int] -> Float
average [] = -1.0
average list = (fromIntegral . sum) list / (fromIntegral . length) list

{-  E 8.14.5 Write a function called countVowels which counts the number of vowels in a string-}

countVowels :: [Char] -> Int
countVowels s = sum (map (\x -> if x `elem` "aeiouAEIOU" then 1 else 0) s)

{-  E 6.14.6 Write a function called addBigs which adds two big numbers given as lists of digits.-}
-- ca bonus, listele pot fi de lungimi diferite

addBigs :: [Int] -> [Int] -> [Int]
addBigs a b =
    let 
    u nr = nr `mod` 10

    helper :: [Int] -> [Int] -> Int -> [Int] -> [Int]
    helper ta tb c res
        | ta == [] && tb == [] = res
        | ta /= [] && tb == [] = if c == 1 then
                            if (head ta) + 1 > 9 then 
                                helper (tail ta) [] 1 ((u ((head ta) + 1)):res)
                            else
                                helper (tail ta) [] 0 (((head ta) + 1):res)
                        else
                            helper (tail ta) [] 0 ((head ta):res)
        | ta == [] && tb /= [] = if c == 1 then
                            if (head tb) + 1 > 9 then
                                helper [] (tail tb) 1 ((u ((head tb) + 1)):res)
                            else
                                helper [] (tail tb) 0 (((head tb) + 1):res)
                        else
                            helper [] (tail tb) 0 ((head tb):res)
        | otherwise = if c == 1 then
                                if (head ta) + (head tb) + 1 > 9 then
                                    helper (tail ta) (tail tb) 1 ((u ((head ta) + (head tb) + 1)):res)
                                else
                                    helper (tail ta) (tail tb) 0 (((head ta)+(head tb)+1):res)
                            else
                                if (head ta) + (head tb) > 9 then
                                    helper (tail ta) (tail tb) 1 ((u ((head ta) + (head tb))):res)
                                else
                                    helper (tail ta) (tail tb) 0 (((head ta) + (head tb)):res)
    in
        helper (reverse a) (reverse b) 0 []


{-  E 8.14.7 To format the result of fact 1000 in LATEX, I had to break the number up in groups of
            80 digits.
            Write a function breakToLines lineLen str with the signature
            breakToLines :: Int -> String -> [String] that takes a string as input an returns
            a list of strings which have length at most lineLen characters.-}

breakToLines :: Int -> String -> [String]
breakToLines c s 
    | c == 0 = [""]
    | otherwise = 
        let 
            helper string acc 
                | string == [] = reverse acc
                | otherwise = helper (drop c string) ((take c string):acc)
        in
            helper s []

formatLines :: [String] -> String
formatLines sl 
    | sl == [] = []
    | otherwise =
        let
            helper s acc
                | s == [] = acc
                | otherwise = helper (tail s) (acc ++ "\n" ++ (head s))
        in
            helper (tail sl) (head sl)