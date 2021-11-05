module L05 exposing (..)

import Lists as L
import Exercises exposing(..)
import Theme exposing(..)

{- 
    E 5.2.1 Trace the evaluation of applyTwice (inc >> triple) 1 , showing each evaluation step. 

    applyTwice (inc >> triple) 1
    applyTwice triple(inc) 1
    triple(inc(triple(inc(1))))
    triple(inc(triple(2)))
    triple(inc(6))
    triple(7)
    21

    E 5.2.1 Implement the all and any functions by using a pipeline with map then foldl .
-}

all : (a -> Bool) -> List a -> Bool
all fn l = L.map fn l |> L.foldl (\x s -> x && s) False 

{-
    Q 5.2.1 Which is the core difference between function composition and pipelines?
    Composition return functions.
    Pipelines return values, if the pipeline begins with one.
-}

{-  E 5.5.1  Write the implementation of the map2: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c 
    function for Maybe .-}

map2: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 f ma mb =
    case ma of
        Just a -> case mb of 
            Just b -> Just (f a b) 
            _ -> Nothing
        _ -> Nothing

{-
    Exercise 5.8.1 Reimplement the countVowels function from the last lab, using pipelines and function
    composition. This implementation should handle lowercase and uppercase vowels too.
    Write at least two tests to check your implementation.
-}

countVowels : String -> Int
countVowels s =
    let
        isVowel vow = (vow == 'a') || (vow == 'e') || (vow == 'i') || (vow == 'o') || (vow == 'u') || (vow == 'A') || (vow == 'E') || (vow == 'I') || (vow == 'O') || (vow == 'U')
    in
        L.foldl (\x st -> x + st) 0 (L.map (\x -> if isVowel x then 1 else 0) (String.toList(s)))

{-
    E 5.8.2 Write a function changePreferenceToDarkTheme : List AccountConfiguration -> List AccountConfiguration
    which receives a list of accounts and returns a list of accounts where the preferredTheme
    field is set to the Dark value.
-}

changePreferenceToDarkTheme : List AccountConfiguration -> List AccountConfiguration
changePreferenceToDarkTheme accounts = accounts |> L.map (\x -> {x | preferredTheme = Dark})

{-
    E 5.8.3 Write a function usersWithPhoneNumbers : List User -> List String which receives a list
    of users and returns a list containing the email addresses the of users who have provided
    a phone number.
-}

usersWithPhoneNumbers : List User -> List String
usersWithPhoneNumbers users = users |> L.filter (\x -> case x.details.phoneNumber of 
    Just _ -> True 
    _ -> False ) |> 
            L.map (\x -> case x.details.phoneNumber of 
            Just res -> res 
            _ -> "Err")

{-
    E 5.8.4 Write a test suite for the chunks function implemented in the last lab. The test suite should include:
        1. Both examples (repeated here for your convenience)
        2. A test for the empty list case
        3. Two more tests of your choice (try to be creative)
-}

reverse : List a -> List a
reverse l =
  let
    reverseAcc lx acc = 
      case lx of
        [] -> acc
        x::xs -> reverseAcc xs (x::acc)
  in
    reverseAcc l []

chunks : Int -> List a -> List (List a)
chunks size list =
    let
        helper n l acc return = 
            case l of
                [] -> return
                x::xs -> if (n == 0) then helper (size - 1) xs [x] (acc :: return) else helper (n - 1) xs (reverse(x :: acc)) return
    in
        reverse(helper size list [] [])