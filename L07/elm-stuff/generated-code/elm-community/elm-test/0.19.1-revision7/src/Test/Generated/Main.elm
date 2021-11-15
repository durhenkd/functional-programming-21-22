module Test.Generated.Main exposing (main)

import Example
import VerifyExamples.Documentation.Combinations0
import VerifyExamples.Documentation.Combinations1
import VerifyExamples.Documentation.Tails0

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    Test.Runner.Node.run
        { runs = 100
        , report = ConsoleReport UseColor
        , seed = 296907707148459
        , processes = 16
        , globs =
            []
        , paths =
            [ "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L07/tests/Example.elm"
            , "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L07/tests/VerifyExamples/Documentation/Combinations0.elm"
            , "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L07/tests/VerifyExamples/Documentation/Combinations1.elm"
            , "/home/durhenkd/Documents/UTCN/An III CTI en - le me/FP - R. R. Slavescu/functional-programming-21-22/L07/tests/VerifyExamples/Documentation/Tails0.elm"
            ]
        }
        [ ( "Example"
          , [ Test.Runner.Node.check Example.suite
            ]
          )
        , ( "VerifyExamples.Documentation.Combinations0"
          , [ Test.Runner.Node.check VerifyExamples.Documentation.Combinations0.spec0
            ]
          )
        , ( "VerifyExamples.Documentation.Combinations1"
          , [ Test.Runner.Node.check VerifyExamples.Documentation.Combinations1.spec1
            ]
          )
        , ( "VerifyExamples.Documentation.Tails0"
          , [ Test.Runner.Node.check VerifyExamples.Documentation.Tails0.spec0
            ]
          )
        ]