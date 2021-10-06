module BlockTest exposing (..)

import Expect
import Markup
import Markup.Block as Block exposing (Block(..), ExprM(..))
import Markup.Meta as Meta
import Markup.Tokenizer as Tokenizer exposing (Lang(..))
import Test exposing (..)


blockTest lang str =
    Markup.parseToBlock lang str



suiteBlock : Test
suiteBlock =
    describe "recovering a substring of the source text from metadata"
        [ Test.only <| test "(1) BlockTest 1 6" <|
            \_ ->
                blockTest MiniLaTeX "foo"
                    |> Expect.equal ( Paragraph [TextM "foo\n" { id = "1.2.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 0 } } }] { begin = 0, end = 1, id = "1.2", indent = 0 })
        ]
