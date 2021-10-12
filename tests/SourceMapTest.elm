module SourceMapTest exposing (suiteSourceMap)

import Expect
import Lang.Lang exposing (Lang(..))
import Markup.Debugger exposing (..)
import Markup.Expr as Markup
import Markup.SourceMap as SourceMap
import Test exposing (..)


makeBlock lang id str =
    Markup.parseToBlock lang id str


block1 =
    makeBlock MiniLaTeX "1.1" 0 "fee"


block2 =
    makeBlock MiniLaTeX "1.2" 2 "foo\nbar\nbaz\nabc"


blocks =
    [ block1, block2 ] |> debug1 "BLOCKS"


source =
    [ "fee\n", "\n", "foo\n", "bar\n", "baz\n", "abc\n" ]


suiteSourceMap : Test
suiteSourceMap =
    describe "recovering a substring of the source text from metadata"
        [ test "(1) find id = 1.2.9" <|
            \_ ->
                SourceMap.find "1.2.9" blocks
                    |> Expect.equal Nothing
        , test "(2) find id = 1.1.0" <|
            \_ ->
                SourceMap.find "1.1.0" blocks
                    |> Expect.equal (Just { id = "1.1.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 0 } } })
        , test
            "(3) find id = 1.2.0"
          <|
            \_ ->
                SourceMap.find "1.2.0" blocks
                    |> Expect.equal (Just { id = "1.2.0", loc = { begin = { col = 0, row = 2 }, end = { col = 3, row = 5 } } })
        , test "(4) get source fragment" <|
            \_ ->
                SourceMap.getFragment "1.1.0" source blocks
                    |> Expect.equal (Just { fragment = "fee\n", meta = { id = "1.1.0", loc = { begin = { col = 0, row = 0 }, end = { col = 3, row = 0 } } } })
        , test "(5) get source fragment" <|
            \_ ->
                SourceMap.getFragment "1.2.0" source blocks
                    |> Expect.equal (Just { fragment = "foo\nbar\nbaz\nabc\n", meta = { id = "1.2.0", loc = { begin = { col = 0, row = 2 }, end = { col = 3, row = 5 } } } })
        ]
