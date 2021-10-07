module BlockParserTest exposing (suiteBlockParser)

import Block.Parser
import Expect
import Markup
import Markup.Block exposing (SBlock(..))
import Markup.Tokenizer as Tokenizer exposing (Lang(..))
import Test exposing (..)


run lang str =
    Block.Parser.run lang 0 (String.lines str) |> .committed


suiteBlockParser : Test
suiteBlockParser =
    describe "recovering a substring of the source text from metadata"
        [ Test.only <|
            test "(1) " <|
                \_ ->
                    run L1 "ABC"
                        |> Expect.equal [ SParagraph [ "ABC" ] { begin = 0, end = 0, id = "0", indent = 0 } ]
        , Test.only <|
            test "(2) " <|
                \_ ->
                    run L1 "ABC\nDEF"
                        |> Expect.equal [ SParagraph [ "ABC", "DEF" ] { begin = 0, end = 1, id = "0", indent = 0 } ]
        , Test.only <|
            test "(3) " <|
                \_ ->
                    run L1 "ABC\nDEF\n\nXYZ"
                        |> Expect.equal [ SParagraph [ "ABC", "DEF", "" ] { begin = 0, end = 2, id = "0", indent = 0 }, SParagraph [ "XYZ" ] { begin = 3, end = 3, id = "1", indent = 0 } ]
        , Test.only <|
            test "(4) " <|
                \_ ->
                    run L1 "ABC\nDEF\n\n\nXYZ"
                        |> Expect.equal [ SParagraph [ "ABC", "DEF", "" ] { begin = 0, end = 2, id = "0", indent = 0 }, SParagraph [ "XYZ" ] { begin = 4, end = 4, id = "1", indent = 0 } ]
        ]
