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
                blockTest MiniLaTeX "1.1" 0 "foo"
                    |> Expect.equal (Paragraph [ TextM "foo\n" { id = "1.1.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 0 } } } ] { begin = 0, end = 1, id = "1.1", indent = 0 })
        , test "(2) BlockTest 1 6" <|
            \_ ->
                blockTest MiniLaTeX "1.2" 2 "foo\nbar\nbaz\nabc"
                    |> Expect.equal (Paragraph [ TextM "foo\nbar\nbaz\nabc\n" { id = "1.2.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 3 } } } ] { begin = 2, end = 6, id = "1.2", indent = 0 })
        ]
