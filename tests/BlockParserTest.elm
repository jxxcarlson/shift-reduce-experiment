module BlockParserTest exposing (suiteL1BlockParser)

import Block.Block exposing (BlockStatus(..), SBlock(..))
import Block.Parser
import Expect
import Lang.Lang exposing (Lang(..))
import Markup.Simplify as Simplify exposing (SBlockS(..))
import Test exposing (..)


run lang str =
    Block.Parser.run lang 0 (String.lines str) |> .committed


run2 lang str =
    Block.Parser.run lang 0 (String.lines str) |> .committed |> List.map Simplify.sblock


suiteL1BlockParser : Test
suiteL1BlockParser =
    describe "parsing L1 blocks"
        [ test "(1) " <|
            \_ ->
                run2 L1 "ABC"
                    |> Expect.equal
                        [ SParagraphS [ "ABC" ] BlockComplete ]
        , test "(2) " <|
            \_ ->
                run2 L1 "ABC\nDEF"
                    |> Expect.equal [ SParagraphS [ "ABC", "DEF" ] BlockComplete ]
        , test "(3) " <|
            \_ ->
                run2 L1 "ABC\nDEF\n\nXYZ"
                    |> Expect.equal
                        [ SParagraphS [ "ABC", "DEF" ] BlockComplete
                        , SParagraphS [ "XYZ" ] BlockComplete
                        ]
        , test "(4) " <|
            \_ ->
                run2 L1 "ABC\nDEF\n\n\nXYZ"
                    |> Expect.equal
                        [ SParagraphS [ "ABC", "DEF" ] BlockComplete
                        , SParagraphS [ "XYZ" ] BlockComplete
                        ]
        , test "(5) " <|
            \_ ->
                run2 L1 "| indent\n   abc\n   def\nxyz"
                    |> Expect.equal
                        [ SBlockS "indent" [ SParagraphS [ "   abc", "   def" ] BlockComplete ] BlockComplete
                        , SParagraphS [ "xyz" ] BlockComplete
                        ]
        , test "(6) " <|
            \_ ->
                run L1 "|| code\n   a[i] = 0"
                    |> Expect.equal [ SVerbatimBlock "code" [ "   a[i] = 0" ] { status = BlockComplete, begin = 0, end = 1, id = "0", indent = 0 } ]
        , test "(7) " <|
            \_ ->
                run L1 "|| code\n   a[i] = 0\n      b[i] = 1\n\nabc"
                    |> Expect.equal
                        [ SVerbatimBlock "code" [ "   a[i] = 0", "      b[i] = 1" ] { begin = 0, end = 2, id = "0", indent = 0, status = BlockComplete }
                        , SParagraph [ "abc" ] { begin = 4, end = 4, id = "1", indent = 0, status = BlockComplete }
                        ]
        , test
            "(8) Nested blocks"
          <|
            \_ ->
                run2 L1 "| foo\n   a\n   b\n   | bar\n      c\n      d\n"
                    |> Expect.equal
                        [ SBlockS "bar" [ SParagraphS [ "      c", "      d" ] BlockComplete ] BlockComplete
                        , SBlockS "foo" [ SParagraphS [ "   a", "   b" ] BlockComplete ] BlockComplete
                        ]
        , test
            "(9) Incomplete block (note the absence of a trailing newline)"
          <|
            \_ ->
                run2 L1 "| foo\n   AAA\n      PQR"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   AAA", "      PQR" ] BlockComplete ] BlockStarted ]
        , test
            "(9) Complete block (note the trailing newline)"
          <|
            \_ ->
                run2 L1 "| foo\n   AAA\n      PQR\n"
                    |> Expect.equal
                        [ SBlockS "foo" [ SParagraphS [ "   AAA", "      PQR" ] BlockComplete ] BlockComplete ]
        ]
