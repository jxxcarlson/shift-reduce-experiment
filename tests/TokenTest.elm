module TokenTest exposing (..)

import Expect
import Parser.Advanced as PA
import ParserTools exposing (StringData)
import SRParser exposing (run)
import Test exposing (..)
import Token exposing (TokenPart(..), parserFromPartsList)


testParser =
    parserFromPartsList [ Char '$', Reject [ '$' ], Char '$' ]


suite : Test
suite =
    describe "The Line modules for MiniLaTex"
        [ test "(1) foo" <|
            \_ ->
                PA.run testParser "$a[i]=1$"
                    |> Expect.equal (Ok [ { begin = 0, content = "$", end = 1 }, { begin = 1, content = "a[i]=1", end = 7 }, { begin = 7, content = "$", end = 8 } ])
        ]
