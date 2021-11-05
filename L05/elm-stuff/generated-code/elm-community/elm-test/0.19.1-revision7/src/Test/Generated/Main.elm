module Test.Generated.Main exposing (main)

import TestSuite

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = JsonReport
        , seed = 373694239261688
        , processes = 16
        , globs =
            []
        , paths =
            [ "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L05/tests/TestSuite.elm"
            ]
        }
        [ ( "TestSuite"
          , [ Test.Runner.Node.check TestSuite.suite
            ]
          )
        ]