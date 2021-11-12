module L06 exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


unravelInHtml : List a 
                -> (List (Attribute msg) -> List (Html msg) -> Html msg) 
                -> (a -> List (Attribute msg)) 
                -> (a -> List (Html msg)) 
                -> List (Html msg)
unravelInHtml list tag attrBuilder htmlBuilder =
    let
        tailHelper l t at ht acc = 
                case l of
                    [] -> acc
                    x::xs -> tailHelper xs t at ht ((t (at x) (ht x) )::acc)
    in
        tailHelper list tag attrBuilder htmlBuilder []


{- E 6.2.1  Starting from the code above and the type definition for Recipe , write a function
            recipeView : Recipe -> Html msg that can render any recipe (i.e. avoid hardcoding the
            recipe data into the view) 
            
            IMPLEMENTED IN RecipeView.elm, it uses the unravelInHtml function above
            with the modification from E 6.4.2
            -}

{- E 6.2.2  Modify the Counter app to prevent the counter from going over 10 or under -10 by
            disabling the + or - buttons when the value is reached.
            Remove the call to skip in the CounterTests.elm file to test your implementation.
            
            IMPLEMENTED IN CounterClass.elm-}

{- E 6.2.3 Modify the Counter app to make the text red when the counter is close (is greater than
            8 or less than -8) to 10 or -10.
            
            IMPLEMENTED IN CounterClass.elm -}

{- E 6.4.1  Write a test for the coin flip app to test that the initial view contains the text “Press the
            flip button to get started”.
            
            IMPLEMENTED IN CoinFlipTests.elm-}

{- Q 6.5.1  What are the 3 components of the Elm Architecture?
            A : update, model, view-}

{- Q 6.5.2  What is the fundamental difference between a command and a message?
            A command is given to the Elm runtime and takes core of things from "the outside world"
            A message is something used by elm internally, everything is a message, from the html layout
            to the value returned by a button press-}

{- Q 6.5.3  Which are the two steps of a command?
            First, the command is given to the elm runime
            Second, we collect the result from the elm runtime-}

{- Q 6.6.1  Modify the Coin flip app to display the number of heads and tails outcomes so far, in two ways:
            1. Keep the number in the Model and simply display it in the view
            2. Compute the values from the flips field of the Model each time in the view
   Q 6.6.2  Modify the Coin flip app to have an initial flip (i.e. when the user loads the app, it should
            initially display heads or tails instead of current message). 
   Q 6.6.3  Add a “Flip 10” and a “Flip 100” button to the Coin flip app that triggers 10 and 100
            coin flips respectively.
            
            ALL IMPLEMENTED IN CoinFlip.elm-}

