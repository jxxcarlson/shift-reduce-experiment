module BlockParserTests2 exposing (run, suiteMiniLaTeXBlockParser)

import Block.Block exposing (BlockStatus(..))
import Block.Parser
import Expect
import Lang.Lang exposing (Lang(..))
import Markup.Simplify as Simplify exposing (SBlockS(..))
import Test exposing (..)


run lang str =
    Block.Parser.run lang 0 (String.lines str) |> .committed |> List.map Simplify.sblock


suiteMiniLaTeXBlockParser : Test
suiteMiniLaTeXBlockParser =
    describe "parsing MiniLaTeX blocks"
        [ test "(1) Two-line paragraph" <|
            \_ ->
                run MiniLaTeX "foo\nbar"
                    |> Expect.equal
                        [ SParagraphS [ "foo", "bar" ] BlockComplete ]
        , test "(2) Two-line paragraph, twice" <|
            \_ ->
                run MiniLaTeX "foo1\nbar1\n\nfoo2\nbar2"
                    |> Expect.equal
                        [ SParagraphS [ "foo1", "bar1" ] BlockComplete
                        , SParagraphS [ "foo2", "bar2" ] BlockComplete
                        ]
        , test "(3) A block" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   abc\n   def\n\\end{foo}"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete ]
        , test "(4) A block with mismatched tags" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   abc\n   def\n\\end{oof}"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] (MismatchedTags "foo" "oof") ]
        , test "(5) An unclosed block" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   abc\n   def"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] (BlockUnfinished "begin") ]

        --[ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete ]
        , test "(6) Two blocks in succession of the same level" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   abc\n   def\n\\end{foo}\n\\begin{bar}\n   xyz\n\\end{bar}"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete
                        , SBlockS "bar" [ SParagraphS [ "   xyz" ] BlockComplete ] BlockComplete
                        ]
        , test "(7) Two blocks in succession of the same level separated by blank lines" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   abc\n   def\n\\end{foo}\n\n\n\\begin{bar}\n   xyz\n\\end{bar}"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete
                        , SBlockS "bar" [ SParagraphS [ "   xyz" ] BlockComplete ] BlockComplete
                        ]
        , test "(8) Two blocks in succession of the same level separated by a paragraph" <|
            \_ ->
                run MiniLaTeX "\\begin{foo}\n   abc\n   def\n\\end{foo}\n\nyada\nnada\n\n\\begin{bar}\n   xyz\n\\end{bar}"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete
                        , SParagraphS [ "yada", "nada" ] BlockComplete
                        , SBlockS "bar" [ SParagraphS [ "   xyz" ] BlockComplete ] BlockComplete
                        ]
        , test "(9) Two verbatim blocks in succession of the same level separated by a paragraph" <|
            \_ ->
                run MiniLaTeX "\\begin{code}\n   abc\n   def\n\\end{code}\n\nyada\nnada\n\n\\begin{math}\n   xyz\n\\end{math}"
                    |> Expect.equal
                        [ SVerbatimBlockS "code" [ "   abc", "   def" ] BlockComplete
                        , SParagraphS [ "yada", "nada" ] BlockComplete
                        , SVerbatimBlockS "math" [ "   xyz" ] BlockComplete
                        ]
        , test "(10) A verbatim block followed by a blank line and a non-blank line" <|
            \_ ->
                run MiniLaTeX "\\begin{code}\n   abc\n   def\n\\end{code}\n\nyada yada"
                    |> Expect.equal
                        [ SVerbatimBlockS "code" [ "   abc", "   def" ] BlockComplete
                        , SParagraphS [ "yada yada" ] BlockComplete
                        ]
        , test "(11) A verbatim block followed by a non-blank line" <|
            \_ ->
                run MiniLaTeX "\\begin{code}\n   abc\n   def\n\\end{code}\nyada yada"
                    |> Expect.equal
                        -- TODO: should be an error since there needs to be a blank line after the code block
                        -- TODO: this kind of error should be handled by the code that assembles SBlocks
                        [ SVerbatimBlockS "code" [ "   abc", "   def" ] BlockComplete ]
        , test "(12) A verbatim with code inside" <|
            \_ ->
                run Markdown "```\n $$\n  x^2\n```  \n"
                    |> Expect.equal
                        [ SVerbatimBlockS "code" [ " $$", "  x^2" ] BlockComplete ]
        , test "(13) A verbatim  block with code inside followed by text at indentation = 0" <|
            \_ ->
                run Markdown "```\n $$\n  x^2\n```\n\nfoo"
                    |> Expect.equal
                        [ SVerbatimBlockS "code" [ " $$", "  x^2" ] BlockComplete, SParagraphS [ "foo" ] BlockComplete ]
        , test "(14) An incomplete verbatim with code inside followed by text at indentation = 0" <|
            \_ ->
                run Markdown "```\n $$\n  x^2\n\nfoo"
                    |> Expect.equal
                        [ SVerbatimBlockS "code" [ " $$", "  x^2" ] (BlockUnfinished "missing end tag"), SParagraphS [ "foo" ] BlockComplete ]
        , test "(15) An incomplete verbatim block" <|
            \_ ->
                run Markdown "$$\n\n"
                    |> Expect.equal
                        [ SVerbatimBlockS "math" [] (BlockUnfinished "missing end tag") ]
        ]
