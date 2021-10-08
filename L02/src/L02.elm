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
    <function> : Adress -> String

    ===== Capitolul cu destructing ma dapaseste teribil ====

    E 2.5.1: Try to remove the last line ( _ -> "Better luck next time" ) and check if the code could be compiled.
    A: It wont, case doesn't cover all the cases

    E 2.5.2:  Try to swap the 1 -> "Gold" and _ -> "Better luck next time" lines. Evaluate the following expressions in the REPL (numberToMedal 1) , (numberToMedal 2) ,
(           numberToMedal 10)
    A: it won't compile since _ -> acts like "default" in C case statements and since it's the first case only that will ever be executed (poor english)

    

-}
