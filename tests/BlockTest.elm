module BlockTest exposing (suiteBlock)

import Expect
import Markup
import Markup.Block as Block exposing (Block(..), ExprM(..))
import Markup.Meta as Meta
import Markup.Tokenizer as Tokenizer exposing (Lang(..))
import Test exposing (..)


blockTest lang id firstLine str =
    Markup.parseToBlock lang id firstLine str


suiteBlock : Test
suiteBlock =
    describe "recovering a substring of the source text from metadata"
        [ test "(1) BlockTest 1 6" <|
            \_ ->
                blockTest MiniLaTeX "1.1" 0 "fee"
                    |> Expect.equal (Paragraph [ TextM "fee\n" { id = "1.1.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 0 } } } ] { begin = 0, end = 1, id = "1.1", indent = 0 })
        , Test.only <|
            test "(2) BlockTest 1 6" <|
                \_ ->
                    blockTest MiniLaTeX "1.2" 0 "foo\nbar"
                        |> Expect.equal (Paragraph [ TextM "foo\nbar\n" { id = "1.2.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 1 } } } ] { begin = 0, end = 2, id = "1.2", indent = 0 })
        ]
