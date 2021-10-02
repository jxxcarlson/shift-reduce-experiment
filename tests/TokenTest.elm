module TokenTest exposing (..)

import Expect
import Parser.Advanced as PA
import ParserTools exposing (StringData)
import SRParser exposing (run)
import Test exposing (..)
import Token exposing (TokenPart(..), parserFromPartsList)
import TokenGrammar


testParser =
    parserFromPartsList [ AcceptChar '$', RejectCharList [ '$' ], AcceptChar '$' ]


suite : Test
suite =
    describe "The Line modules for MiniLaTex"
        [ test "(1) foo" <|
            \_ ->
                PA.run testParser "$a[i]=1$"
                    |> Expect.equal (Ok [ { begin = 0, content = "$", end = 1 }, { begin = 1, content = "a[i]=1", end = 7 }, { begin = 7, content = "$", end = 8 } ])
        , test "(2) rejectCharsParser" <|
            \_ ->
                PA.run TokenGrammar.rejectCharsParser "<[]>foo"
                    |> Expect.equal (Ok (Just (RejectCharList [ ']', '[' ])))
        , test "(3) rejectCharsParser" <|
            \_ ->
                PA.run TokenGrammar.rejectCharsParser "<[>foo"
                    |> Expect.equal (Ok (Just (RejectChar '[')))
        , test "(4) acceptCharsParser" <|
            \_ ->
                PA.run TokenGrammar.acceptCharsParser "!2"
                    |> Expect.equal (Ok (Just (AcceptCharList [ '!', '2' ])))
        , test "(5) acceptCharsParser" <|
            \_ ->
                PA.run TokenGrammar.acceptCharsParser "!"
                    |> Expect.equal (Ok (Just (AcceptChar '!')))
        ]
