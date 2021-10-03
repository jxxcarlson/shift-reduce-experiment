module ParserTests exposing (..)

import Expect
import L1 exposing (Expr(..))
import SRParser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "The Line modules for MiniLaTex"
        [ test "(1) foo" <|
            \_ ->
                run "foo"
                    |> Expect.equal { committed = [ L1Text "foo" ], end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) foo [i ABC]" <|
            \_ ->
                run "foo [i ABC]"
                    |> Expect.equal { committed = [ L1Text "foo ", Expr "i" [ L1Text "ABC" ] ], end = 11, scanPointer = 11, sourceText = "foo [i ABC]", stack = [] }
        , test "(3) [i [j ABC]]" <|
            \_ ->
                run "foo [i [j ABC]]"
                    |> Expect.equal { committed = [ L1Text "foo ", Expr "i" [ Expr "j" [ L1Text "ABC" ] ] ], end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }
        , test "(4) [i ABC] [j DEF]" <|
            \_ ->
                run "foo [i ABC] [j DEF]"
                    |> Expect.equal { committed = [ L1Text "foo ", Expr "i" [ L1Text "ABC" ], L1Text " ", Expr "j" [ L1Text "DEF" ] ], end = 19, scanPointer = 19, sourceText = "foo [i ABC] [j DEF]", stack = [] }
        , test "(5) [i foo (ERROR: missing right bracket)" <|
            \_ ->
                run "[i foo"
                    |> Expect.equal { committed = [ L1Text "I corrected an unmatched '[' in the following expression: ", Expr "i" [ L1Text "foo" ] ], end = 6, scanPointer = 6, sourceText = "[i foo", stack = [] }
        , test "(6) foo [i bar] [j UUU (ERROR: missing right bracket)" <|
            \_ ->
                run "foo [i bar] [j UUU"
                    |> Expect.equal { committed = [ L1Text "foo ", Expr "i" [ L1Text "bar" ], L1Text " ", L1Text "I corrected an unmatched '[' in the following expression: ", Expr "j" [ L1Text "UUU" ] ], end = 18, scanPointer = 18, sourceText = "foo [i bar] [j UUU", stack = [] }
        , test "(7) foo [i bar [j UUU] (ERROR: missing right bracket)" <|
            \_ ->
                run "foo [i bar [j UUU]"
                    |> .committed
                    |> Expect.equal [ L1Text "foo ", L1Text "Error! I added a bracket after this: [i bar [j UUU]", Expr "i bar" [ Expr "j" [ L1Text "UUU" ] ] ]
        ]
