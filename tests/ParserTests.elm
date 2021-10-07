module ParserTests exposing (suiteL1, suiteMarkdown, suiteMiniLaTeX)

import Expect
import Markup.AST exposing (Expr(..))
import Markup.Markup exposing (run)
import Markup.Tokenizer exposing (Lang(..))
import Test exposing (..)


loc i j =
    { begin = i, end = j }


suiteMarkdown : Test
suiteMarkdown =
    describe "parsing Markdown"
        [ test "(1) foo" <|
            \_ ->
                run Markdown "foo"
                    |> Expect.equal { committed = [ Text "foo" (loc 0 2) ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) BOLD" <|
            \_ ->
                run Markdown "*foo*"
                    |> Expect.equal { committed = [ Expr "strong" [ Text "foo" (loc 0 4) ] (loc 0 4) ], count = 2, end = 5, scanPointer = 5, sourceText = "*foo*", stack = [] }
        , test "(3) LINK" <|
            \_ ->
                run Markdown "[N Y T](url)"
                    |> Expect.equal { committed = [ Expr "link" [ Text "N Y T" (loc 0 6), Text "url" (loc 7 11) ] (loc 0 11) ], count = 3, end = 12, scanPointer = 12, sourceText = "[N Y T](url)", stack = [] }
        ]


suiteMiniLaTeX : Test
suiteMiniLaTeX =
    describe "parsing MiniLaTeX"
        [ test "(1) foo" <|
            \_ ->
                run MiniLaTeX "foo"
                    |> Expect.equal { committed = [ Text "foo" (loc 0 2) ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) \\foo" <|
            \_ ->
                run MiniLaTeX "\\foo"
                    |> Expect.equal { committed = [ Expr "foo" [] (loc 0 3) ], count = 2, end = 4, scanPointer = 4, sourceText = "\\foo", stack = [] }
        , test "(3) \\foo{1}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}"
                    |> Expect.equal { committed = [ Expr "foo" [ Text "1" (loc 5 5) ] (loc 0 6) ], count = 5, end = 7, scanPointer = 7, sourceText = "\\foo{1}", stack = [] }
        , test "(4) \\foo{1}{2}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}{2}"
                    |> Expect.equal { committed = [ Expr "foo" [ Text "1" (loc 5 5), Text "2" (loc 8 8) ] (loc 0 9) ], count = 8, end = 10, scanPointer = 10, sourceText = "\\foo{1}{2}", stack = [] }
        , test "(5) abc \\foo{1} def" <|
            \_ ->
                run MiniLaTeX "abc \\foo{1} def"
                    |> Expect.equal { committed = [ Text "abc " (loc 0 3), Expr "foo" [ Text "1" (loc 9 9) ] (loc 4 10), Text " def" (loc 11 14) ], count = 7, end = 15, scanPointer = 15, sourceText = "abc \\foo{1} def", stack = [] }
        , test "(6) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "\\foo{\\bar{1}}"
                    |> Expect.equal { committed = [ Expr "foo" [ Expr "bar" [ Text "1" (loc 10 10) ] (loc 5 11) ] (loc 0 12) ], count = 8, end = 13, scanPointer = 13, sourceText = "\\foo{\\bar{1}}", stack = [] }
        , test "(7) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "$x^2$"
                    |> Expect.equal { committed = [ Verbatim "math" "$x^2$" (loc 0 4) ], count = 2, end = 5, scanPointer = 5, sourceText = "$x^2$", stack = [] }
        , test "(8)  abc \\href{1}{2} def" <|
            \_ ->
                run MiniLaTeX "\\href{1}{2}"
                    |> Expect.equal { committed = [ Expr "href" [ Text "1" { begin = 6, end = 6 }, Text "2" { begin = 9, end = 9 } ] { begin = 0, end = 10 } ], count = 8, end = 11, scanPointer = 11, sourceText = "\\href{1}{2}", stack = [] }
        , test "(9) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "abc \\href{1}{2} def"
                    |> Expect.equal { committed = [ Text "abc " { begin = 0, end = 3 }, Expr "href" [ Text "1" { begin = 10, end = 10 }, Text "2" { begin = 13, end = 13 } ] { begin = 4, end = 14 }, Text " def" { begin = 15, end = 18 } ], count = 10, end = 19, scanPointer = 19, sourceText = "abc \\href{1}{2} def", stack = [] }
        ]


suiteL1 : Test
suiteL1 =
    describe "parsing L1"
        [ test "(1) foo" <|
            \_ ->
                run L1 "foo"
                    |> Expect.equal { committed = [ Text "foo" (loc 0 2) ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) foo [i ABC]" <|
            \_ ->
                run L1 "foo [i ABC]"
                    |> Expect.equal { committed = [ Text "foo " (loc 0 3), Expr "i" [ Text "ABC" (loc 4 10) ] (loc 4 10) ], count = 5, end = 11, scanPointer = 11, sourceText = "foo [i ABC]", stack = [] }
        , test "(3) [i [j ABC]]" <|
            \_ ->
                run L1 "foo [i [j ABC]]"
                    |> Expect.equal { committed = [ Text "foo " (loc 0 3), Expr "i" [ Expr "j" [ Text "ABC" (loc 7 13) ] (loc 7 13) ] (loc 4 14) ], count = 8, end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }
        , test "(4) [i ABC] [j DEF]" <|
            \_ ->
                run L1 "foo [i ABC] [j DEF]"
                    |> Expect.equal { committed = [ Text "foo " (loc 0 3), Expr "i" [ Text "ABC" (loc 4 10) ] (loc 4 10), Text " " (loc 11 11), Expr "j" [ Text "DEF" (loc 12 18) ] (loc 12 18) ], count = 9, end = 19, scanPointer = 19, sourceText = "foo [i ABC] [j DEF]", stack = [] }
        , test "(5) [i foo (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "[i foo"
                    |> Expect.equal { committed = [ Text "I corrected an unmatched '[' in the following expression: " (loc 0 0), Expr "i" [ Text "foo" (loc 0 5) ] (loc 0 5) ], count = 4, end = 6, scanPointer = 6, sourceText = "[i foo", stack = [] }
        , test "(6) foo [i bar] [j UUU (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "foo [i bar] [j UUU"
                    |> Expect.equal { committed = [ Text "foo " (loc 0 3), Expr "i" [ Text "bar" (loc 4 10) ] (loc 4 10), Text " " (loc 11 11), Text "I corrected an unmatched '[' in the following expression: " (loc 0 0), Expr "j" [ Text "UUU" (loc 12 17) ] (loc 12 17) ], count = 9, end = 18, scanPointer = 18, sourceText = "foo [i bar] [j UUU", stack = [] }
        , test "(7) foo [i bar [j UUU] (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "foo [i bar [j UUU]"
                    |> .committed
                    |> Expect.equal [ Text "foo " (loc 0 3), Text "Error! I added a bracket after this: [i bar [j UUU]" (loc 0 0), Expr "i bar" [ Expr "j" [ Text "UUU" (loc 11 17) ] (loc 11 17) ] (loc 4 19) ]
        ]
