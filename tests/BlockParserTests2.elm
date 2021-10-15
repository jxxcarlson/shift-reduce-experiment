module BlockParserTests2 exposing (..)

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.Parser
import Expect
import Lang.Lang exposing (Lang(..))
import Markup.Simplify as Simplify exposing (SBlockS(..))
import Test exposing (..)


run lang str =
    Block.Parser.run lang 0 (String.lines str) |> .committed |> List.map Simplify.sblock


suiteL1BlockParser : Test
suiteL1BlockParser =
    describe "parsing L1 blocks"
        [ test "(1) Two-line paragraph" <|
            \_ ->
                run MiniLaTeX "foo\nbar"
                    |> Expect.equal
                        [ SParagraphS [ "foo", "bar" ] BlockComplete ]
        , test "(2) Two-line paragraph, twice" <|
            \_ ->
                run MiniLaTeX "foo\nbar\n\nfoo\nbar"
                    |> Expect.equal
                        [ SParagraphS [ "foo", "bar", "" ] BlockComplete
                        , SParagraphS [ "foo", "bar" ] BlockComplete
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
        , Test.skip <|
            test "(5) An unclosed block" <|
                \_ ->
                    run MiniLaTeX "\\begin{foo}\n   abc\n   def"
                        |> Expect.notEqual
                            [ SBlockS "foo" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete ]
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
                        , SParagraphS [ "yada", "nada", "" ] BlockComplete
                        , SBlockS "bar" [ SParagraphS [ "   xyz" ] BlockComplete ] BlockComplete
                        ]
        , test "(9) Two verbatim blocks in succession of the same level separated by a paragraph" <|
            \_ ->
                run MiniLaTeX "\\begin{code}\n   abc\n   def\n\\end{code}\n\nyada\nnada\n\n\\begin{math}\n   xyz\n\\end{math}"
                    |> Expect.equal
                        [ SVerbatimBlockS "code" [ "   abc", "   def" ] BlockComplete
                        , SParagraphS [ "yada", "nada", "" ] BlockComplete
                        , SVerbatimBlockS "math" [ "   xyz" ] BlockComplete
                        ]
        ]
