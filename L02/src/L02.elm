module L02 exposing (..)

x = 42

{-
    E 2.2.1: Try to call the function with an argument such that "Dr. Haskell Curry" is displayed
    A: fullTitle {firstName = "Haskell", idDr = True, lstName = "Curry"}

    E 2.3.1: Call the fullName function using the User type constructor. Did you encounter any errors?
    A: fullName (User "Haskell" "Curry") . This works, i encountered errors without the paranthesis, obviously

    Q 2.3.1: Does the way type alias works remind you of any keyword in C and C++?
    A: typedef

    E 2.3.2: Define a type alias Address , which includes 4 fields: street, number, city and country.
    A:
    > type alias Address = {street: String, number: Int, city: String, country: String}
    > Address
    <function> : String -> Int -> String -> String -> Adress

    E 2.3.3: Write a function formatAddress , which takes an instance of an Address and displays it as street number, city, country.
    A:
    > formatAddress : Address -> String
    | formatAddress paddress = paddress.street ++ " " ++ String.fromInt(paddress.number) ++ " " ++ paddress.city ++ " " ++ paddress.country
    |   
    <function> : Address -> String

    E 2.5.1: Try to remove the last line ( _ -> "Better luck next time" ) and check if the code could be compiled.
    A: It wont, case doesn't cover all the cases

    E 2.5.2:  Try to swap the 1 -> "Gold" and _ -> "Better luck next time" lines. Evaluate the following expressions in the REPL (numberToMedal 1) , (numberToMedal 2) ,
(           numberToMedal 10)
    A: it won't compile since _ -> acts like "default" in C case statements and since it's the first case only that will ever be executed (poor english)

    
    Q 2.7.1: What is the cardinality of the Bool type?
    A: 2

    Q 2.7.2: How would you define Int as a sum type? Is the definition valid Elm syntax?
    A: You would have to list every value Int can have. -2147483648 | -2147483647 | -2147483648 ... | 2147483647

    Q 2.7.3: What are the built-in types that have cardinality 1 and 0, respectively? Can you define
        such types (i.e. will the compiler allow it)? What is the use case for such types?
    A: 

-}

{-   E 2.8.1 Define a type for a dice which has six sides   -}
type Dice = One | Two | Three | Four | Five | Six

{-   E 2.8.2 Define a type DicePair , which contains 2 Dice , in two ways, one using type aliases and one using type definitions.  -}

type DicePair1 = DicePair1 Dice Dice
type alias DicePair2 = {dice1 : Dice, dice2 : Dice}
type alias DicePair3 = (Dice, Dice)

{-   E 2.8.3 Write a function luckyRoll which takes a DicePair and returns a String . It should
        return “Very lucky” if the roll contains 2 sixes, “Lucky” it contains one six and “Meh”
        otherwise.  
    A:
    
luckyRoll : DicePair1 -> String
| luckyRoll (DicePair1 dice1 dice2) = 
|   if dice1 == Six && dice2 == Six then
|     "Very Lucky"
|   else if dice1 == Six || dice2 == Six then
|     "Lucky"
|   else
|     "Meh"
|   

-}


{-   E 2.8.4 Write the function areaRec for ShapeRec.  

type ShapeRec = 
|   CircleRec { radius : Float }
|   RectangleRec { width : Float, height : Float }
|   TriangleRec { sideA : Float, sideB : Float, sideC : Float }
|  
-}
heron : Float -> Float -> Float -> Float
heron a b c =
   let
     s = (a + b + c) / 2
   in
     sqrt (s * (s - a) * (s - b) * (s - c))
   

{-
A:

> areaRec : ShapeRec -> Float
| areaRec shape = 
|   case shape of
|     CircleRec {radius} -> pi * radius * radius
|     RectangleRec {width, height} -> width * height
|     TriangleRec {sideA, sideB, sideC} -> heron sideA sideB sideC
|   


-}

{-
    E 2.8.5: Using the declarations for Point and Shape2D write a function pointInShape , which
        determines if a given point is inside a given shape.
-}

type alias Point = {x: Float, y: Float}

type Shape2D
    = Circle {center: Point, radius: Float}
    | Rectangle {topLeftCorner: Point, bottomRightCorner: Point}
    | Triangle {pointA: Point, pointB: Point, pointC: Point}

pointInShape : Point -> Shape2D -> Bool
pointInShape point shape = 
    let
        distance {x1, y1} {x2, y2} = sqrt((x1 - x2)^2 + (y1 - y2)^2)
        
        area point1 point2 point3 = heron (distance {x1 = point1.x, y1 = point1.y} {x2 = point2.x, y2 = point2.y}) 
                                            (distance {x1 = point1.x, y1 = point1.y} {x2 = point3.x, y2 = point3.y}) 
                                            (distance {x1 = point3.x, y1 = point3.y} {x2 = point2.x, y2 = point2.y})
    in
        case shape of
           Circle {center, radius} -> distance {x1 = point.x, y1 = point.y}  {x2 = center.x, y2 = center.x} < radius 
           Rectangle {topLeftCorner, bottomRightCorner} -> point.x > topLeftCorner.x && point.y < topLeftCorner.y && point.x < bottomRightCorner.x && point.y > bottomRightCorner.y
           Triangle {pointA, pointB, pointC} -> (area pointA pointB pointC) == ((area point pointA pointB) + (area point pointA pointC) + (area point pointB pointC))

{-

    > pointInShape (Point 1 1) (Circle {center = (Point 0 0), radius = 2})
    True : Bool
    > pointInShape (Point 1 1) (Circle {center = (Point 0 0), radius = 1})
    False : Bool

    > pointInShape (Point 0.5 0.3) (Rectangle {topLeftCorner = (Point -1 1), bottomRightCorner = (Point 1 -1)})
    True : Bool
    > pointInShape (Point 0.5 0.3) (Rectangle {topLeftCorner = (Point -1 1), bottomRightCorner = (Point 0 -1)})
    False : Bool


-}

