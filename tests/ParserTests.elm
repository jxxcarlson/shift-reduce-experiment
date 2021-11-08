module ParserTests exposing (suiteL1, suiteMarkdown, suiteMiniLaTeX)

import Block.Block exposing (BlockStatus(..))
import Either exposing (..)
import Expect
import Expression.AST exposing (Expr(..))
import Expression.Parser exposing (run)
import Expression.Token as Token
import Lang.Lang exposing (Lang(..))
import Markup.API as API
import Markup.Simplify as Simplify exposing (BlockS(..), ExprS(..))
import Test exposing (..)


p : Lang -> String -> List Simplify.BlockS
p lang str =
    API.parse lang 0 (String.lines str) |> .ast |> Simplify.blocks



-- |> Simplify.expressions


loc i j =
    { begin = i, end = j }


suiteMarkdown : Test
suiteMarkdown =
    describe "parsing Markdown"
        [ test "(1) foo" <|
            \_ ->
                run Markdown "foo"
                    |> Expect.equal { committed = [ Text "foo" { begin = 0, end = 2 } ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [], tokenStack = [] }
        , test "(2) BOLD" <|
            \_ ->
                run Markdown "*foo*"
                    |> Expect.equal { committed = [ Expr "strong" [ Text "foo" (loc 0 4) ] (loc 0 4) ], count = 2, end = 5, scanPointer = 5, sourceText = "*foo*", stack = [], tokenStack = [] }
        , test "(3) LINK" <|
            \_ ->
                run Markdown "[N Y T](url)"
                    |> Expect.equal { committed = [ Expr "link" [ Text "N Y T" { begin = 1, end = 5 }, Text "url" { begin = 8, end = 10 } ] { begin = 7, end = 11 } ], count = 7, end = 12, scanPointer = 12, sourceText = "[N Y T](url)", stack = [], tokenStack = [] }
        , test "(4) nested functions" <|
            \_ ->
                run Markdown "[!italic]([!gray](This is a way to de-emphasize text.))"
                    |> Expect.equal { committed = [ Expr "italic" [ Expr "gray" [ Text "This is a way to de-emphasize text." { begin = 18, end = 52 } ] { begin = 17, end = 53 } ] { begin = 9, end = 54 } ], count = 12, end = 55, scanPointer = 55, sourceText = "[!italic]([!gray](This is a way to de-emphasize text.))", stack = [], tokenStack = [] }
        , test "(5)  function, text" <|
            \_ ->
                run Markdown "[nyt](URL) boo!"
                    |> Expect.equal
                        { committed = [ Expr "link" [ Text "nyt" { begin = 1, end = 3 }, Text "URL" { begin = 6, end = 8 } ] { begin = 5, end = 9 }, Text " boo!" { begin = 10, end = 14 } ], count = 8, end = 15, scanPointer = 15, sourceText = "[nyt](URL) boo!", stack = [], tokenStack = [] }
        , test "(6)  two functions in text" <|
            \_ ->
                run Markdown "colors [!blue](BLUE) and [!violet](VIOLET) also"
                    |> Expect.equal
                        { committed = [ Text "colors " { begin = 0, end = 6 }, Expr "blue" [ Text "BLUE" { begin = 15, end = 18 } ] { begin = 14, end = 19 }, Text " and " { begin = 20, end = 24 }, Expr "violet" [ Text "VIOLET" { begin = 35, end = 40 } ] { begin = 34, end = 41 }, Text " also" { begin = 42, end = 46 } ], count = 16, end = 47, scanPointer = 47, sourceText = "colors [!blue](BLUE) and [!violet](VIOLET) also", stack = [], tokenStack = [] }
        , test "(7)  two functions in text" <|
            \_ ->
                run Markdown "[!italic](Foo [!strong](Bar) Baz)"
                    |> Expect.equal
                        { committed = [ Expr "italic" [ Text "Foo " { begin = 10, end = 13 }, Expr "strong" [ Text "Bar" { begin = 24, end = 26 } ] { begin = 23, end = 27 }, Text " Baz" { begin = 28, end = 31 } ] { begin = 9, end = 32 } ], count = 14, end = 33, scanPointer = 33, sourceText = "[!italic](Foo [!strong](Bar) Baz)", stack = [], tokenStack = [] }
        , test "(8)  two functions in text" <|
            \_ ->
                run Markdown "[!italic]([stuff)\n"
                    |> Expect.equal
                        { committed = [ Text "\n" { begin = 17, end = 17 }, Text "Error! I added a bracket after this: " { begin = 0, end = 0 } ], count = 9, end = 18, scanPointer = 18, sourceText = "[!italic]([stuff)\n", stack = [ Left (Token.Symbol "]" { begin = 18, end = 19 }), Left (Token.Symbol ")" { begin = 16, end = 16 }), Left (Token.Text "stuff" { begin = 11, end = 15 }), Left (Token.Symbol "[" { begin = 10, end = 10 }), Left (Token.Symbol "(" { begin = 9, end = 9 }), Right (Expr "italic" [] { begin = 0, end = 8 }) ], tokenStack = [] }
        ]


suiteMiniLaTeX : Test
suiteMiniLaTeX =
    describe "parsing MiniLaTeX"
        [ test "(1) foo" <|
            \_ ->
                run MiniLaTeX "foo"
                    |> Expect.equal { committed = [ Text "foo" (loc 0 2) ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [], tokenStack = [] }
        , test "(2) \\foo" <|
            \_ ->
                run MiniLaTeX "\\foo"
                    |> .committed
                    |> Simplify.expressions
                    |> Expect.equal [ ExprS "blue" [ TextS "\\foo" ], ExprS "errorHighlight" [ ExprS "red" [ TextS "{??}" ] ] ]
        , test "(3) \\foo{1}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}"
                    |> Expect.equal { committed = [ Expr "foo" [ Text "1" (loc 5 5) ] (loc 0 6) ], count = 5, end = 7, scanPointer = 7, sourceText = "\\foo{1}", stack = [], tokenStack = [] }
        , test "(4) \\foo{1}{2}" <|
            \_ ->
                run MiniLaTeX "\\foo{1}{2}"
                    |> Expect.equal { committed = [ Expr "foo" [ Text "1" (loc 5 5), Text "2" (loc 8 8) ] (loc 0 9) ], count = 8, end = 10, scanPointer = 10, sourceText = "\\foo{1}{2}", stack = [], tokenStack = [] }
        , test "(5) abc \\foo{1} def" <|
            \_ ->
                run MiniLaTeX "abc \\foo{1} def"
                    |> Expect.equal { committed = [ Text "abc " (loc 0 3), Expr "foo" [ Text "1" (loc 9 9) ] (loc 4 10), Text " def" (loc 11 14) ], count = 7, end = 15, scanPointer = 15, sourceText = "abc \\foo{1} def", stack = [], tokenStack = [] }
        , test "(6) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "\\foo{\\bar{1}}"
                    |> Expect.equal { committed = [ Expr "foo" [ Expr "bar" [ Text "1" (loc 10 10) ] (loc 5 11) ] (loc 0 12) ], count = 8, end = 13, scanPointer = 13, sourceText = "\\foo{\\bar{1}}", stack = [], tokenStack = [] }
        , test "(7) \\foo{\\bar{1}}" <|
            \_ ->
                run MiniLaTeX "$x^2$"
                    |> Expect.equal { committed = [ Verbatim "math" "x^2" (loc 0 4) ], count = 2, end = 5, scanPointer = 5, sourceText = "$x^2$", stack = [], tokenStack = [] }
        , test "(8)  abc \\href{1}{2} def" <|
            \_ ->
                run MiniLaTeX "\\href{1}{2}"
                    |> Expect.equal { committed = [ Expr "href" [ Text "1" { begin = 6, end = 6 }, Text "2" { begin = 9, end = 9 } ] { begin = 0, end = 10 } ], count = 8, end = 11, scanPointer = 11, sourceText = "\\href{1}{2}", stack = [], tokenStack = [] }
        , test "(9) abc \\href{1}{2} def" <|
            \_ ->
                run MiniLaTeX "abc \\href{1}{2} def"
                    |> Expect.equal { committed = [ Text "abc " { begin = 0, end = 3 }, Expr "href" [ Text "1" { begin = 10, end = 10 }, Text "2" { begin = 13, end = 13 } ] { begin = 4, end = 14 }, Text " def" { begin = 15, end = 18 } ], count = 10, end = 19, scanPointer = 19, sourceText = "abc \\href{1}{2} def", stack = [], tokenStack = [] }
        , test "(10) nested macros" <|
            \_ ->
                run MiniLaTeX "\\italic{AAA \\bold{BBB} }"
                    |> Expect.equal { committed = [ Expr "italic" [ Text "AAA " { begin = 8, end = 11 }, Expr "bold" [ Text "BBB" { begin = 18, end = 20 } ] { begin = 12, end = 21 }, Text " " { begin = 22, end = 22 } ] { begin = 0, end = 23 } ], count = 10, end = 24, scanPointer = 24, sourceText = "\\italic{AAA \\bold{BBB} }", stack = [], tokenStack = [] }
        , test "(11) href + text" <|
            \_ ->
                run MiniLaTeX "\\href{1}{2} XYZ"
                    |> Expect.equal
                        { committed = [ Expr "href" [ Text "1" { begin = 6, end = 6 }, Text "2" { begin = 9, end = 9 } ] { begin = 0, end = 10 }, Text " XYZ" { begin = 11, end = 14 } ], count = 9, end = 15, scanPointer = 15, sourceText = "\\href{1}{2} XYZ", stack = [], tokenStack = [] }
        , test "(12) text, macro, math" <|
            \_ ->
                run MiniLaTeX "foo \\italic{bar} $x$"
                    |> Expect.equal
                        { committed = [ Text "foo " { begin = 0, end = 3 }, Expr "italic" [ Text "bar" { begin = 12, end = 14 } ] { begin = 4, end = 15 }, Text " " { begin = 16, end = 16 }, Verbatim "math" "x" { begin = 17, end = 19 } ], count = 8, end = 20, scanPointer = 20, sourceText = "foo \\italic{bar} $x$", stack = [], tokenStack = [] }
        , test "(13)  nested macro, macro, text" <|
            \_ ->
                run MiniLaTeX "\\italic{AAA \\strong{BBB} CCC}\n"
                    |> Expect.equal { committed = [ Expr "italic" [ Text "AAA " { begin = 8, end = 11 }, Expr "strong" [ Text "BBB" { begin = 20, end = 22 } ] { begin = 12, end = 23 }, Text " CCC" { begin = 24, end = 27 } ] { begin = 0, end = 28 }, Text "\n" { begin = 29, end = 29 } ], count = 11, end = 30, scanPointer = 30, sourceText = "\\italic{AAA \\strong{BBB} CCC}\n", stack = [], tokenStack = [] }
        ]


suiteL1 : Test
suiteL1 =
    describe "parsing L1"
        [ test "(1) foo" <|
            \_ ->
                run L1 "foo"
                    |> Expect.equal { committed = [ Text "foo" (loc 0 2) ], count = 2, end = 3, scanPointer = 3, sourceText = "foo", stack = [], tokenStack = [] }
        , test "(2) foo [i ABC]" <|
            \_ ->
                p L1 "foo [i ABC]"
                    |> Expect.equal [ ParagraphS [ TextS "foo ", ExprS "italic" [ TextS "ABC" ], TextS "\n" ] BlockComplete ]
        , test "(3) foo [i [j ABC]] (composition)" <|
            \_ ->
                p L1 "foo [i [j ABC]]"
                    |> Expect.equal [ ParagraphS [ TextS "foo ", ExprS "italic" [ ExprS "j" [ TextS "ABC" ] ], TextS "\n" ] BlockComplete ]
        , test "(4) [i ABC] [j DEF]" <|
            \_ ->
                p L1 "[i ABC] [j DEF]"
                    |> Expect.equal [ ParagraphS [ ExprS "italic" [ TextS "ABC" ], TextS " ", ExprS "j" [ TextS "DEF" ], TextS "\n" ] BlockComplete ]
        , test "(5) [i foo (ERROR: missing right bracket)" <|
            \_ ->
                p L1 "[i foo"
                    |> Expect.equal [ ParagraphS [ TextS "Error! I added a bracket after this: [i foo\n", ExprS "italic" [ TextS "foo\n" ] ] BlockComplete ]
        , test "(6) foo [i bar] [j UUU (ERROR: missing right bracket)" <|
            \_ ->
                p L1 "foo [i bar] [j UUU"
                    |> Expect.equal [ ParagraphS [ TextS "foo ", ExprS "italic" [ TextS "bar" ], TextS " ", TextS "Error! I added a bracket after this: [j UUU\n", ExprS "j" [ TextS "UUU\n" ] ] BlockComplete ]
        , test "(7) foo [i bar [j UUU] (ERROR: missing right bracket)" <|
            \_ ->
                p L1 "foo [i bar [j UUU]"
                    |> Expect.equal [ ParagraphS [ TextS "foo ", TextS "Error! I added a bracket after this: [i bar [j UUU]\n" ] BlockComplete ]
        ]
