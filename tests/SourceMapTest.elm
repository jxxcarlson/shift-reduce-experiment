module SourceMapTest exposing (suiteSourceMap)

import Expect
import Markup
import Markup.Block as Block exposing (Block(..), ExprM(..))
import Markup.Meta as Meta
import Markup.SourceMap as SourceMap
import Markup.Tokenizer as Tokenizer exposing (Lang(..))
import Test exposing (..)


makeBlock lang id str =
    Markup.parseToBlock lang id str


block1 =
    makeBlock MiniLaTeX "1.1" 0 "fee"


block2 =
    makeBlock MiniLaTeX "1.2" 2 "foo\nbar\nbaz\nabc"


blocks =
    [ block1, block2 ]


source =
    [ "fee", "", "foo", "bar", "baz", "abc" ]


b1 =
    Paragraph [ TextM "foo\n" { id = "1.1.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 0 } } } ] { begin = 0, end = 1, id = "1.1", indent = 0 }


b2 =
    Paragraph [ TextM "foo\nbar\nbaz\nabc\n" { id = "1.2.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 3 } } } ] { begin = 0, end = 4, id = "1.2", indent = 0 }


suiteSourceMap : Test
suiteSourceMap =
    describe "recovering a substring of the source text from metadata"
        [ test "(1) find id = 1.2.9" <|
            \_ ->
                SourceMap.find "1.2.9" blocks
                    |> Expect.equal Nothing
        , test "(2) find id = 1.2.0" <|
            \_ ->
                SourceMap.find "1.2.0" blocks
                    |> Expect.equal (Just { id = "1.2.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 3 } } })
        ]
