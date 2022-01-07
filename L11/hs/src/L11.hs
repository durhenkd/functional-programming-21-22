
module L11 where

{- 
    E 11.8.1 Implement the Functor instance for the data Tree a = Nil | Node (Tree a) a (Tree a) type.
    Note: i also implemented the Show type so i can check the restults

    Ex:
    *L11> copac1 = Node (Node Nil 2 Nil) 1 (Node Nil 3 (Node Nil 68 Nil))
    *L11> fmap (+1) copac1
-}

data Tree a = Nil | Node (Tree a) a (Tree a)

instance (Show a) => Show (Tree a) where
    show Nil = "Nil"
    show (Node l v r) = "( Node " ++ (show l) ++ " " ++ (show v) ++ " " ++ (show r) ++ " )"

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

{-
    E 11.8.2 Implement a function passwords that enumerates all n character passwords containing
    digits (0 - 9) lowercase letters (a - z) and uppercase letters (A - Z) using the list applicative.
-}

passwords :: Int -> [String]
passwords n = sequenceA $ replicate n "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

{-
    E 11.8.3 Write an interactive Haskell program that asks a user for their weight (in kilograms) and
    height (in meters) and calculates their BMI using the following formula

    Done in: BMI.hs
-}

{-
    E 11.8.4 Write a new version of the program developed in Exercise 11.8.3 that uses command line
    arguments instead of reading from the standard input.
    You can assume that all the inputs will be valid.

    Done in: BMIArgs.hs
-}

{-
    E 11.8.5 Write a program that given a text encrypted with the Caesar cipher and a known word
    from the plaintext, will try to decrypt the message using brute force (by trying all possible
    decryptions). The known word will be provided as a command line argument and the
    ciphertext (encrypted message) will be read from the standard input.

    Done in: CaesarWord.hs
-}

