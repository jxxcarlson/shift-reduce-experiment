module ParserTests exposing (suiteL1, suiteMarkdown, suiteMiniLaTeX)

import Expect
import Markup.AST exposing (Expr(..))
import Markup.Token as Token
import Markup.Tokenizer exposing (Lang(..))
import MarkupParser exposing (run)
import Test exposing (..)


suiteMarkdown : Test
suiteMarkdown =
    describe "parsing Markdown"
        [ test "(1) foo" <|
            \_ ->
                run Markdown "foo"
                    |> Expect.equal { committed = [ Text "foo" Token.dummyLoc ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) BOLD" <|
            \_ ->
                run Markdown "*foo*"
                    |> Expect.equal { committed = [ Expr "strong" [ Text "foo" Token.dummyLoc ] Token.dummyLoc ], count = 2, end = 5, scanPointer = 5, sourceText = "*foo*", stack = [] }
        , test "(3) LINK" <|
            \_ ->
                run Markdown "[N Y T](url)"
                    |> Expect.equal { committed = [ Expr "link" [ Text "N Y T" Token.dummyLoc, Text "url" Token.dummyLoc ] Token.dummyLoc ], count = 3, end = 12, scanPointer = 12, sourceText = "[N Y T](url)", stack = [] }
        ]


suiteMiniLaTeX : Test
suiteMiniLaTeX =
    describe "parsing MiniLaTeX"
        [ test "(1) foo" <|
            \_ ->
                run MiniLaTeX "foo"
                    |> Expect.equal { committed = [ Text "foo" Token.dummyLoc ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) \\foo" <|
            \_ ->
                run MiniLaTeX "\\foo"
                    |> Expect.equal { committed = [ Expr "foo" [] Token.dummyLoc ], count = 2, end = 4, scanPointer = 4, sourceText = "\\foo", stack = [] }
        , test "(3) \\foo{1}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}"
                    |> Expect.equal { committed = [ Expr "foo" [ Text "1" Token.dummyLoc ] Token.dummyLoc ], count = 5, end = 7, scanPointer = 7, sourceText = "\\foo{1}", stack = [] }
        , test "(4) \\foo{1}{2}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}{2}"
                    |> Expect.equal { committed = [ Expr "foo" [ Text "1" Token.dummyLoc, Text "2" Token.dummyLoc ] Token.dummyLoc ], count = 8, end = 10, scanPointer = 10, sourceText = "\\foo{1}{2}", stack = [] }
        , test "(5) abc \\foo{1} def" <|
            \_ ->
                run MiniLaTeX "abc \\foo{1} def"
                    |> Expect.equal { committed = [ Text "abc " Token.dummyLoc, Expr "foo" [ Text "1" Token.dummyLoc ] Token.dummyLoc, Text " def" Token.dummyLoc ], count = 7, end = 15, scanPointer = 15, sourceText = "abc \\foo{1} def", stack = [] }
        , test "(6) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "\\foo{\\bar{1}}"
                    |> Expect.equal { committed = [ Expr "foo" [ Expr "bar" [ Text "1" Token.dummyLoc ] Token.dummyLoc ] Token.dummyLoc ], count = 8, end = 13, scanPointer = 13, sourceText = "\\foo{\\bar{1}}", stack = [] }
        , test "(7) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "$x^2$"
                    |> Expect.equal { committed = [ Verbatim "math" "$x^2$" Token.dummyLoc ], count = 2, end = 5, scanPointer = 5, sourceText = "$x^2$", stack = [] }
        ]


suiteL1 : Test
suiteL1 =
    describe "parsing L1"
        [ test "(1) foo" <|
            \_ ->
                run L1 "foo"
                    |> Expect.equal { committed = [ Text "foo" Token.dummyLoc ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [] }
        , test "(2) foo [i ABC]" <|
            \_ ->
                run L1 "foo [i ABC]"
                    |> Expect.equal { committed = [ Text "foo " Token.dummyLoc, Expr "i" [ Text "ABC" Token.dummyLoc ] Token.dummyLoc ], count = 5, end = 11, scanPointer = 11, sourceText = "foo [i ABC]", stack = [] }
        , test "(3) [i [j ABC]]" <|
            \_ ->
                run L1 "foo [i [j ABC]]"
                    |> Expect.equal { committed = [ Text "foo " Token.dummyLoc, Expr "i" [ Expr "j" [ Text "ABC" Token.dummyLoc ] Token.dummyLoc ] Token.dummyLoc ], count = 8, end = 15, scanPointer = 15, sourceText = "foo [i [j ABC]]", stack = [] }
        , test "(4) [i ABC] [j DEF]" <|
            \_ ->
                run L1 "foo [i ABC] [j DEF]"
                    |> Expect.equal { committed = [ Text "foo " Token.dummyLoc, Expr "i" [ Text "ABC" Token.dummyLoc ] Token.dummyLoc, Text " " Token.dummyLoc, Expr "j" [ Text "DEF" Token.dummyLoc ] Token.dummyLoc ], count = 9, end = 19, scanPointer = 19, sourceText = "foo [i ABC] [j DEF]", stack = [] }
        , test "(5) [i foo (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "[i foo"
                    |> Expect.equal { committed = [ Text "I corrected an unmatched '[' in the following expression: " Token.dummyLoc, Expr "i" [ Text "foo" Token.dummyLoc ] Token.dummyLoc ], count = 4, end = 6, scanPointer = 6, sourceText = "[i foo", stack = [] }
        , test "(6) foo [i bar] [j UUU (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "foo [i bar] [j UUU"
                    |> Expect.equal { committed = [ Text "foo " Token.dummyLoc, Expr "i" [ Text "bar" Token.dummyLoc ] Token.dummyLoc, Text " " Token.dummyLoc, Text "I corrected an unmatched '[' in the following expression: " Token.dummyLoc, Expr "j" [ Text "UUU" Token.dummyLoc ] Token.dummyLoc ], count = 9, end = 18, scanPointer = 18, sourceText = "foo [i bar] [j UUU", stack = [] }
        , test "(7) foo [i bar [j UUU] (ERROR: missing right bracket)" <|
            \_ ->
                run L1 "foo [i bar [j UUU]"
                    |> .committed
                    |> Expect.equal [ Text "foo " Token.dummyLoc, Text "Error! I added a bracket after this: [i bar [j UUU]" Token.dummyLoc, Expr "i bar" [ Expr "j" [ Text "UUU" Token.dummyLoc ] Token.dummyLoc ] Token.dummyLoc ]
        ]
