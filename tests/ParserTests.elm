module ParserTests exposing (..)

import Either exposing (Either(..))
import Expect exposing (Expectation)
import Grammar exposing (GExpr(..))
import SRParser exposing (run)
import Test exposing (..)
import Tokenizer exposing (Token(..))


suite : Test
suite =
    describe "The Line modules for MiniLaTex"
        [ test "(1) foo" <|
            \_ ->
                run "foo"
                    |> Expect.equal { committed = [ GText "foo" ], end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) foo [i ABC]" <|
            \_ ->
                run "foo [i ABC]"
                    |> Expect.equal { committed = [ GText "foo ", GExpr "i" [ GText "ABC" ] ], end = 11, scanPointer = 11, sourceText = "foo [i ABC]", stack = [] }
        , test "(3) [i [j ABC]]" <|
            \_ ->
                run "foo [i [j ABC]]"
                    |> Expect.equal { committed = [ GText "foo ", GExpr "i" [ GExpr "j" [ GText "ABC" ] ] ], end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }
        , test "(4) [i ABC] [j DEF]" <|
            \_ ->
                run "foo [i ABC] [j DEF]"
                    |> Expect.equal { committed = [ GText "foo ", GExpr "i" [ GText "ABC" ], GText " ", GExpr "j" [ GText "DEF" ] ], end = 19, scanPointer = 19, sourceText = "foo [i ABC] [j DEF]", stack = [] }
        , test "(5) [i foo (ERROR: missing right bracket)" <|
            \_ ->
                run "[i foo"
                    |> Expect.equal { committed = [ GText "I corrected an unmatched '[' in the following expression: ", GExpr "i" [ GText "foo" ] ], end = 6, scanPointer = 6, sourceText = "[i foo", stack = [] }
        ]
