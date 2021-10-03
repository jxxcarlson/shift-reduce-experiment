module ParserTests exposing (suiteL1, suiteMiniLaTeX)

import AST exposing (Expr(..))
import Expect
import SRParser exposing (run)
import Test exposing (..)
import Tokenizer exposing (Lang(..))


suiteMiniLaTeX : Test
suiteMiniLaTeX =
    describe "parsing MiniLaTeX"
        [ test "(1) foo" <|
            \_ ->
                run MiniLaTeX "foo"
                    |> Expect.equal { committed = [ Text "foo" ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) \\foo" <|
            \_ ->
                run MiniLaTeX "\\foo"
                    |> Expect.equal { committed = [ Expr "\\foo" [] ], count = 2, end = 4, scanPointer = 4, sourceText = "\\foo", stack = [] }
        , test "(3) \\foo{1}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}"
                    |> Expect.equal { committed = [ Expr "\\foo" [ Text "1" ] ], count = 5, end = 7, scanPointer = 7, sourceText = "\\foo{1}", stack = [] }
        , test "(4) \\foo{1}{2}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}{2}"
                    |> Expect.equal { committed = [ Expr "\\foo" [ Text "1", Text "2" ] ], count = 8, end = 10, scanPointer = 10, sourceText = "\\foo{1}{2}", stack = [] }
        , test "(5) abc \\foo{1} def" <|
            \_ ->
                run MiniLaTeX "abc \\foo{1} def"
                    |> Expect.equal { committed = [ Text "abc ", Expr "\\foo" [ Text "1" ], Text " def" ], count = 7, end = 15, scanPointer = 15, sourceText = "abc \\foo{1} def", stack = [] }
        ]


suiteL1 : Test
suiteL1 =
    describe "parsing L1"
        [ test "(1) foo" <|
            \_ ->
                run L1 "foo"
                    |> Expect.equal { committed = [ Text "foo" ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) foo [i ABC]" <|
            \_ ->
                run L1 "foo [i ABC]"
                    |> Expect.equal { committed = [ Text "foo ", Expr "i" [ Text "ABC" ] ], count = 5, end = 11, scanPointer = 11, sourceText = "foo [i ABC]", stack = [] }
        , test "(3) [i [j ABC]]" <|
            \_ ->
                run L1 "foo [i [j ABC]]"
                    |> Expect.equal { committed = [ Text "foo ", Expr "i" [ Expr "j" [ Text "ABC" ] ] ], count = 8, end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }
        , test "(4) [i ABC] [j DEF]" <|
            \_ ->
                run L1 "foo [i ABC] [j DEF]"
                    |> Expect.equal { committed = [ Text "foo ", Expr "i" [ Text "ABC" ], Text " ", Expr "j" [ Text "DEF" ] ], count = 9, end = 19, scanPointer = 19, sourceText = "foo [i ABC] [j DEF]", stack = [] }
        , test "(5) [i foo (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "[i foo"
                    |> Expect.equal { committed = [ Text "I corrected an unmatched '[' in the following expression: ", Expr "i" [ Text "foo" ] ], count = 4, end = 6, scanPointer = 6, sourceText = "[i foo", stack = [] }
        , test "(6) foo [i bar] [j UUU (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "foo [i bar] [j UUU"
                    |> Expect.equal { committed = [ Text "foo ", Expr "i" [ Text "bar" ], Text " ", Text "I corrected an unmatched '[' in the following expression: ", Expr "j" [ Text "UUU" ] ], count = 9, end = 18, scanPointer = 18, sourceText = "foo [i bar] [j UUU", stack = [] }
        , test "(7) foo [i bar [j UUU] (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "foo [i bar [j UUU]"
                    |> .committed
                    |> Expect.equal [ Text "foo ", Text "Error! I added a bracket after this: [i bar [j UUU]", Expr "i bar" [ Expr "j" [ Text "UUU" ] ] ]
        ]
