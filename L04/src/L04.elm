module L04 exposing (..)

enumerate : List a -> List (Int, a)
enumerate list =
    let
        helper index list1 =
            case list1 of
                [] -> []
                x::xs -> (index , x) :: helper (index+1) xs
    in
        helper 0 list

repeat : Int -> a -> List a
repeat n elem =
    case n of
        0 -> []
        _ -> elem :: repeat (n - 1) elem

{-countVowels : String -> Int
countVowels letters =
    let-}
