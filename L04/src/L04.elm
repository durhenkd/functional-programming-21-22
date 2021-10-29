module L04 exposing (..)

import Date as D

{-
    Q 4.1.1 How can we prevent a type from being instantiated in uncontrolled ways in Java?
    A:  I really don't know

    Q 4.4.1 Are foldr and foldl tail recursive?
    A: Yes


-}

{- E 4.6.1  Implement a function enumerate , with the signature enumerate : List a -> List (Int, a)
            which returns a list of tuples, where the first member of the tuple is the index of the
            element in the list and second member is the element at that position in the list. -}

enumerate : List a -> List (Int, a)
enumerate list =
    let
        helper index list1 return =
            case list1 of
                [] -> return
                x::xs -> helper (index+1) xs ((index , x) :: return)
    in
        helper 0 list []

{- E 4.6.2  Implement a function repeat n elem , with the signature repeat : Int -> a -> List a which
            returns a list that contains elem n times. -}

repeat : Int -> a -> List a
repeat n elem  =
    let
        repeatTail repeats e return = 
            case repeats of
                0 -> return
                _ -> repeatTail (repeats - 1) e (e :: return)
    in
        repeatTail n elem []
    

{- E 4.6.3  Implement a function countVowels , with the signature countVowels : String -> Int which
            returns the number of vowels in a String .-}

countVowels : String -> Int
countVowels s =
    let
        isVowel vow = (vow == 'a') || (vow == 'e') || (vow == 'i') || (vow == 'o') || (vow == 'u')
        countVHelper letterList acc =
            case letterList of
               [] -> acc
               x::xs -> if (isVowel x) then countVHelper xs (acc + 1) else countVHelper xs acc
    in
        countVHelper (String.toList(s)) 0

{- E 4.6.4 Implement the partition function without using any other functions defined on lists (i.e.
            from scratch).-}

{-
partition : comparable -> List comparable -> (List comparable, List comparable)
partition pivot l =
    (filter (\x -> x < pivot) l, filter (\x -> x >= pivot) l)
-}

partition : comparable -> List comparable -> (List comparable, List comparable)
partition pivot list =
    let
        jesus l less greater =
            case l of
               [] -> (less, greater)
               x::xs -> if (x < pivot) then jesus xs (x :: less) greater else jesus xs less (x :: greater)
    in 
        jesus list [] []

{- E 4.6.5 Write a function with the signature
        countriesWithCapital : List (String, String) -> (String -> Bool) -> List String that takes a list of
        (Country, Capital) tuples and returns the list of countries whose capital city name
        matches a given predicate. -}

countriesWithCapital : List (String, String) -> (String -> Bool) -> List String 
countriesWithCapital list pred = 
    let
        helper l return = 
            case l of
            [] -> return
            (country, capital)::xs -> if (pred capital) then helper xs (country :: return) else helper xs return
    in
        helper list []

{- E 4.6.6 Implement a function with the signature filterMap : (a -> Maybe b) -> List a -> List b which
        combines the functionality of filter and map : it applies the function received as first
        arguments to each element an only keeps the elements that are wrapped in the Just -}

{-  NEEDED FUNCTIONS -}
map : (a -> b) -> List a -> List b
map fn l =
  case l of 
    [] -> []
    x::xs -> (fn x)::map fn xs

reverse : List a -> List a
reverse l =
  let
    reverseAcc lx acc = 
      case lx of
        [] -> acc
        x::xs -> reverseAcc xs (x::acc)
  in
    reverseAcc l []

{- ACTUAL ANSWER -}

filterMap : (a -> Maybe b) -> List a -> List b
filterMap f list = 
    let
        helper mapped return = 
            case mapped of
               [] -> return
               (Just a)::xs -> helper xs (a :: return)
               _::xs -> helper xs return
    in
        reverse (helper (map f list) [])

{- E 4.6.7 Implement the all and any functions by using only foldl .-}

{-  NEEDED FUNCTIONS -}
foldl : (a -> b -> b) -> b -> List a -> b
foldl op start l =
  case l of
    [] -> start
    x::xs -> foldl op (op x start) xs

{- ACTUAL ANSWER -}

all : (a -> Bool) -> List a -> Bool
all pred l = foldl (\a b -> a && b) True (map (\x -> pred x) l)

{- E 4.6.8 Implement a function chunks n l with the signature chunks: Int -> List a -> List (List a)
        which splits the list l in chunks of length n .-}

chunks : Int -> List a -> List (List a)
chunks size list =
    let
        helper n l acc return = 
            case l of
                [] -> return
                x::xs -> if (n == 0) then helper (size - 1) xs [x] (acc :: return) else helper (n - 1) xs (reverse(x :: acc)) return
    in
        reverse(helper size list [] [])



{- E 4.6.9 You may have noticed that the createDate function is not always correct: it doesn’t handle leap years!
            1. Modify the Date.elm module, by adding a new function with the signature
                isLeapYear : Int -> Bool , that checks if a given year between 1970 and 3000 is a leap year.
            2. Then modify the daysInMonth function to account for leap years.
            3. Finally update the createDate function to use the corrected version of daysInMonth 
            
            Hint: You should change the signature to daysInMonth : Month -> Int -> Int , where the second
                parameter is the year. -}

{- SOLVED IN DATE.ELM -}


{- E 4.6.10 Implement the enumerate by using only foldl .-}

{- E 4.6.11 Implement a function collect l with the signature
        collect : List (Result err ok) -> Result err (List ok) which takes a list of Result s and
        returns the first element that is in the Err variant, or if all elements are in the Ok
        variant, then list of all unwrapped values, with the list wrapped in the Ok variant.-}