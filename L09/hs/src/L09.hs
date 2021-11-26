
module L09 where

{-  E 9.4.1 What will be printed by the :sprint l after running the following commands: 
    A: 
    > l = 1 : 3 : 5 : _ 
    the sprint command doesn't force avaluation, so all that hass been 
    evaluated from l are 3 element, thanks to the take 3 expression earlier-}

{-  E 9.4.2 What will be printed by the :sprint l after running the following commands:
    A: 
    Prelude> l = [1,10..] :: [Int]
    Prelude> take 3 (filter (\x -> mod x 10 == 0) l)
    [10,100,190]
    Prelude> :sprint l
    l = 1 : 10 : 19 : 28 : 37 : 46 : 55 : 64 : 73 : 82 : 91 : 100 :
    109 : 118 : 127 : 136 : 145 : 154 : 163 : 172 : 181 : 190 : _
    -}

{-  Q 9.7.1 Of the following functions, which are guaranteed to always terminate, even on infinite
lists?
    1. sort     NO
    2. take     YES
    3. min (find the minimum value)             NO
    4. elem (check if a value is in the list)   NO
    5. head         YES
    6. filter       NO
    -}

{-  E 9.8.1 Write a function called cycl :: [a] -> [a] that takes list as parameter and repeats the
            list infinitely-}

cycl :: [a] -> [a]
cycl list = 
    case list of
        [] -> []
        otherwise -> list ++ cycl list

{-  E 9.8.2 Write a function called series :: [[Int]] that generates the following list:
            [[1], [2, 1], [3, 2, 1], ...]
            -}

series :: [[Int]]
series = 
    let
        addMaxPlus :: [Int] -> [Int]
        addMaxPlus list = ((maximum list)+1):list
    in
        [1]:(map addMaxPlus series)

{-  E 9.8.3 Write a function called iter :: (a -> a) -> a -> [a] which takes a function
            f and a starting value a and returns an infinite list which contains:
            [a, f a, f (f a), f (f (f a)), ...]
    -}

iter :: (a -> a) -> a -> [a]
iter f n =
    n:(map f (iter f n))

{-  E 9.8.4 -}

