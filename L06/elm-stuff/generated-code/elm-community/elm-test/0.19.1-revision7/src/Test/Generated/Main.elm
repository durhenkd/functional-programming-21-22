module Test.Generated.Main exposing (main)

import CoinFlipTests
import CounterTests
import RecipeTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 107752000877282
        , processes = 16
        , globs =
            []
        , paths =
            [ "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L06/tests/CoinFlipTests.elm"
            , "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L06/tests/CounterTests.elm"
            , "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L06/tests/RecipeTests.elm"
            ]
        }
        [ ( "CoinFlipTests"
          , [ Test.Runner.Node.check CoinFlipTests.initialViewTest
            ]
          )
        , ( "CounterTests"
          , [ Test.Runner.Node.check CounterTests.viewHasTwoButtons
            , Test.Runner.Node.check CounterTests.viewContainsTheCurrentCount
            , Test.Runner.Node.check CounterTests.buttonDisabledOver10
            ]
          )
        , ( "RecipeTests"
          , [ Test.Runner.Node.check RecipeTests.atLeastOneIngredient
            , Test.Runner.Node.check RecipeTests.atLeastOneIngredientClass
            , Test.Runner.Node.check RecipeTests.eachIngredientHasClassIngredient
            ]
          )
        ]