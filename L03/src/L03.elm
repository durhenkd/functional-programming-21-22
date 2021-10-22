module L03 exposing (..)
import Html exposing (a)
import List exposing (reverse)
import Html.Attributes exposing (list)

{-
    Q 3.1.1 What Java concept is the equivalent to type variables? What about C++?
    A: Generic classes, or Class objects (used in java reflection) for java.
    And templates in c++;

    Q 3.1.2 Can we constrain type variables in Java or C++? If yes how?
    A: We can in Java, when creating generic classes we specify which class must be extended like:
    class ListObject<T extends Comparable<? super T>>{...}

    Q 3.2.1 How is equality handled for float types? Try to evaluate in the REPL: (0/0) and then
    (0/0) == (0/0) . Does this cause a problem for reference equality?

    > (0/0)
    NaN : Float
    > (0/0) == (0/0)
    False : Bool

    A: no, actually, it's good that it returns false

    Q 3.3.1 What is known as the “billion dollar mistake” in Computer Science?
    A: null is the misguided invention of British computer scientist Tony Hoare 
    (most famous for his Quicksort algorithm) in 1964, 
    who coined his invention of null references as his 
    “billion dollar mistake”.

    Q 3.3.2 Can we know at compile time if any given pointer is null or not in C?
            Are C++ references different?
    A: since pointers are essentially variables, no. C++ references are different, since they refer
    to initialised variables, which can be checked at compile time.

    Q 3.3.3 Do you know any language that has the solution to the nullability problem built-in as
            Language feature?
    A: Elm lmao. Dart.

    Q 3.3.4 Discuss at least 2 advantages and disadvantages of each approach. In which cases would
        you use one over the other?
    A: String errors are more user friendly, and you can append extra messages
        i would use string errors for functions that interract with the user
       Enumerated errors are easier to use if we do error handling
        i would use enumerated errors for local functions or functions
        not visible to other programmers/user

    E 3.4.1 Write a function len that returns that length of a list (i.e., the number of elements in it).
-}

len : List a -> Int
len list = 
    case list of
        [] -> 0
        _::xs -> 1 + len xs


{-
    Q 3.4.1 What is the time complexity of the following operations on a singly linked list:
    A:
        1. Insert at the list beginning (head)  O(1)
        2. Insert at the list end (tail)        O(n)
        3. Get the ith element                  O(n)?O(i)

    E 3.4.2 Find two values for b , b1 and b2 such that b2 = b1 + 1 and countFromTo 1 b overflows for
            b2, but it doesn’t for b1.
    A: ?? exercise from the first laboratory?
        "4940 and 4941. Of course, they seem to me arbitrary values. I would have loved to see how actually big is the call stack size, but every google search
        takes e to "Maximum call stack size exceeded" error. However, i estimate it should be roughly 120kB."

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

{-
    Q 3.4.2 What is the algorithmic complexity of the appendTail function?
    A:  
-}

{- E 3.5.1 Write a function with the signature safeDiv : Int -> Int -> Maybe Int that returns
Nothing when we try to divide by 0 and the result wrapped in Just otherwise. -}

safeDiv : Int -> Int -> Maybe Int
safeDiv a b = 
    if b == 0 then
        Nothing
        else
    Just (a // b)

{- E 3.5.2 Rewrite the len function defined above in a tail-recursive style, with the name lenTail  -}

lenTail : List a -> Int
lenTail list = 
    let
        lenHelper l acc = 
            case l of
               [] -> acc
               _::xs -> lenHelper xs (acc + 1)
    in
        lenHelper list 0



{- E 3.5.3 Implement a function last that returns the last element of a list. -}

lastElement : List a -> Maybe a
lastElement list = 
    case list of
       [] -> Nothing
       x::xs -> if xs == [] then Just x else lastElement xs


{- E 3.5.4 Write a function indexList i l which returns the ith element of the list l . -}

indexList : List a -> Int -> Maybe a
indexList list index = 
    let
        indexHelper l i acc = 
            case l of
                [] -> Nothing
                x::xs -> if (i == acc) then Just x else indexHelper xs i (acc + 1)
    in
        indexHelper list index 0

{- E 3.5.5 Write a function fibs start end , which takes a two numbers, start and end and returns
            a list of the Fibonacci numbers such that:
            fibs(start,end) = {fib(i)|i ∈N,start ≤i < end} -}


{- IT'S TAIL RECURSIVE TOO-}
fibs : Int -> Int -> List Int
fibs start end = 
    let
        fib x y i acc = 
            if i == 0 then
                acc
            else
                fib y (x + y) (i - 1) (y :: acc)

        cutAt list index =
            if index == 0 then
                list
            else
                case list of
                    [] -> []
                    _::xs -> cutAt xs (index - 1)
    in
        cutAt (reverse(fib 0 1 end [])) start

{- E 3.5.6 Modify the fibs function to return a list of tuples, where the first element in each tuple
    denotes the index of the Fibonacci number and the second the Fibonacci number itself.-}

fibsT : Int -> Int -> List (Int, Int)
fibsT start end = 
    let
        fib x y i c acc = 
            if i == 0 then
                acc
            else
                fib y (x + y) (i - 1) (c + 1) ((c , y) :: acc)

        cutAt list index =
            if index == 0 then
                list
            else
                case list of
                    [] -> []
                    _::xs -> cutAt xs (index - 1)
    in
        cutAt (reverse(fib 0 1 end 0 [])) start

{- E 3.5.7 Write a function with the signature cmpShapes : Shape -> Shape -> Result String Order that uses
            the safeArea function to calculate the area of the 2 input shapes and returns the ordering
            between them wrapped in the Ok variant, if the area of both shapes can be calculated.
            Otherwise it should return an error message wrapped in the Err variant. -}

{---------------------------------------- NEEDED FUNCTIONS AND TYPES ----------------------------------------}
type Shape
  = Circle Float
  | Rectangle Float Float
  | Triangle Float Float Float

heron : Float -> Float -> Float -> Float
heron a b c =
  let
    s = (a + b + c) / 2
  in
    sqrt (s * (s - a) * (s - b) * (s - c))


validTriangle : number -> number -> number -> Bool
validTriangle a b c =
  ((a > 0) && (b > 0) && (c > 0)) &&
  ((a + b >= c) && (a + c >= b) && (b + c >= a))

safeHeron : Float -> Float -> Float -> Maybe Float
safeHeron a b c =
  if not (validTriangle a b c) then
    Nothing
  else
    Just (heron a b c)

safeArea : Shape -> Result String Float
safeArea shape =
  case shape of
    Circle radius ->
      if radius < 0 then
        Err "Negative circle radius"
      else
        Ok (pi * radius * radius)
    Rectangle width height ->
      if (width < 0) || (height < 0) then
        Err "Negative rectangle width or height"
      else
        Ok (width * height)
    Triangle a b c ->
      case safeHeron a b c of
        Just area -> Ok area
        Nothing -> Err "Sides can't form a triangle"

{---------------------------------------- ACTUAL SOLUTION ----------------------------------------}

cmpShapes : Shape -> Shape -> Result String Order
cmpShapes shape1 shape2 =
    let
        area1 = safeArea shape1
        area2 = safeArea shape2
    in
        case area1 of
           Err string -> Err ("Invalid LEFT shape: " ++ string)
           Ok res1 -> case area2 of
                        Err string -> Err ("Invalid RIGHT shape: " ++ string)
                        Ok res2 -> if (res1 > res2) then Ok GT else if (res2 > res1) then Ok LT else Ok EQ

{- E 3.5.8 Write a function with the signature: totalArea : List Shape -> Result (Int, InvalidShapeError) Float
            that uses the safeAreaEnum function to calculate the total area of the list input shapes. It
            should returns total area (the sum of all areas) in the Ok variant, if all the areas can be
            calculated. Otherwise it should return a tuple with the error returned by safeAreaEnum
            and the index of the shape that caused the error wrapped in the Err variant.-}

{---------------------------------------- NEEDED FUNCTIONS AND TYPES ----------------------------------------}

type InvalidShapeError
  = InvalidCircle
  | InvalidRectangle InvalidRectangleError
  | InvalidTriangle InvalidTriangleError

type InvalidRectangleError
  = InvalidWidth
  | InvalidHeight

type InvalidTriangleError
 = NegativeSide TriangleSide
 | ImpossibleTriangle

type TriangleSide = A | B | C

safeAreaEnum : Shape -> Result InvalidShapeError Float
safeAreaEnum shape =
  case shape of
    Circle radius ->
      if radius < 0 then
        Err InvalidCircle
      else
        Ok (pi * radius * radius)
    Rectangle width height ->
      if (width < 0) then
        Err (InvalidRectangle InvalidWidth)
      else if (height < 0) then
        Err (InvalidRectangle InvalidHeight)
      else
        Ok (width * height)
    Triangle a b c -> 
      case safeHeronEnum a b c of
        Ok area -> Ok area
        Err err -> Err (InvalidTriangle err)

safeHeronEnum : Float -> Float -> Float -> Result InvalidTriangleError Float
safeHeronEnum a b c =
  if (a < 0) then
    Err (NegativeSide A)
  else if (b < 0) then
    Err (NegativeSide B)
  else if (c < 0) then
    Err (NegativeSide C)
  else if ((a + b < c) || (a + c < b) || (b + c < a)) then
    Err ImpossibleTriangle
  else Ok (heron a b c)

  {---------------------------------------- ACTUAL SOLUTION ----------------------------------------}

totalArea : List Shape -> Result (Int, InvalidShapeError) Float
totalArea list = 
    let
        adder l index acc =
            case l of
               [] -> Ok acc
               (x::xs) -> case safeAreaEnum x of 
                            Err error -> Err (index, error)
                            Ok result -> adder xs (index + 1) (acc + result)
    in
        adder list 0 0