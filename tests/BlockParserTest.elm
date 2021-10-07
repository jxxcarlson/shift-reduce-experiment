module BlockParserTest exposing (suiteL1BlockParser)

import Block.Parser
import Expect
import Markup.Block exposing (SBlock(..))
import Markup.Tokenizer exposing (Lang(..))
import Test exposing (..)


run lang str =
    Block.Parser.run lang 0 (String.lines str) |> .committed


suiteL1BlockParser : Test
suiteL1BlockParser =
    describe "parsing L1 blocks"
        [ test "(1) " <|
            \_ ->
                run L1 "ABC"
                    |> Expect.equal [ SParagraph [ "ABC" ] { begin = 0, end = 0, id = "0", indent = 0 } ]
        , test "(2) " <|
            \_ ->
                run L1 "ABC\nDEF"
                    |> Expect.equal [ SParagraph [ "ABC", "DEF" ] { begin = 0, end = 1, id = "0", indent = 0 } ]
        , test "(3) " <|
            \_ ->
                run L1 "ABC\nDEF\n\nXYZ"
                    |> Expect.equal [ SParagraph [ "ABC", "DEF", "" ] { begin = 0, end = 2, id = "0", indent = 0 }, SParagraph [ "XYZ" ] { begin = 3, end = 3, id = "1", indent = 0 } ]
        , test "(4) " <|
            \_ ->
                run L1 "ABC\nDEF\n\n\nXYZ"
                    |> Expect.equal [ SParagraph [ "ABC", "DEF", "" ] { begin = 0, end = 2, id = "0", indent = 0 }, SParagraph [ "XYZ" ] { begin = 4, end = 4, id = "1", indent = 0 } ]
        , test "(5) " <|
            \_ ->
                run L1 "| indent\n   abc\n   def\nxyz"
                    |> Expect.equal [ SBlock "indent" [ SParagraph [ "   abc", "   def" ] { begin = 1, end = 2, id = "1", indent = 3 } ] { begin = 0, end = 2, id = "0", indent = 0 }, SParagraph [ "xyz" ] { begin = 3, end = 3, id = "1", indent = 0 } ]
        , test "(6) " <|
            \_ ->
                run L1 "|| code\n   a[i] = 0"
                    |> Expect.equal [ SVerbatimBlock "code" [ "   a[i] = 0" ] { begin = 0, end = 1, id = "0", indent = 0 } ]
        , test "(7) " <|
            \_ ->
                run L1 "|| code\n   a[i] = 0\n      b[i] = 1\n\nabc"
                    |> Expect.equal [ SVerbatimBlock "code" [ "   a[i] = 0", "      b[i] = 1" ] { begin = 0, end = 2, id = "0", indent = 0 }, SParagraph [ "abc" ] { begin = 4, end = 4, id = "1", indent = 0 } ]
        ]


suiteMiniLaTeXBlockParser : Test
suiteMiniLaTeXBlockParser =
    describe "parsing MiniLaTeX blocks"
        [ test "(1) " <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   a\n   b\n\\end{foo}"
                    |> Expect.equal [ SBlock "foo" [ SParagraph [ "   a", "   b" ] { begin = 1, end = 2, id = "1", indent = 3 } ] { begin = 0, end = 3, id = "0", indent = 0 } ]
        , test "(2) Nested environments" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   xyz\n   \\begin{bar}\n      abc\n   \\end{bar}\\end{foo}"
                    |> Expect.equal [ SBlock "foo" [ SParagraph [ "   xyz" ] { begin = 1, end = 1, id = "1", indent = 3 } ] { begin = 0, end = 1, id = "0", indent = 0 }, SBlock "bar" [ SParagraph [ "      abc" ] { begin = 3, end = 3, id = "3", indent = 6 } ] { begin = 2, end = 3, id = "1", indent = 3 } ]
        ]


suiteMarkdownBlockParser : Test
suiteMarkdownBlockParser =
    describe "parsing Markdown blocks"
        [ test "(1) " <|
            \_ ->
                run Markdown "# Intro to Chemistry"
                    |> Expect.equal [ SBlock "foo" [ SParagraph [ "   a", "   b" ] { begin = 1, end = 2, id = "1", indent = 3 } ] { begin = 0, end = 3, id = "0", indent = 0 } ]
        , test "(2) " <|
            \_ ->
                run Markdown "- foo bar"
                    |> Expect.equal [ SBlock "item" [ SParagraph [ "foo bar" ] { begin = 0, end = 0, id = "0", indent = 0 } ] { begin = 0, end = 0, id = "0", indent = 0 } ]
        , test "(3) " <|
            \_ ->
                run Markdown "> foo bar\n   abc"
                    |> Expect.equal [ SBlock "quotation" [ SParagraph [ "foo bar", "   abc" ] { begin = 0, end = 1, id = "0", indent = 0 } ] { begin = 0, end = 1, id = "0", indent = 0 } ]
        ]
