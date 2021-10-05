module MetaDataTests exposing (..)

import Expect
import Markup.Meta as Meta
import Test exposing (..)


metaDataTest : Int -> Int -> String -> { accept : Bool, input : String, output : String, meta : Meta.ExpressionMeta }
metaDataTest begin end content =
    let
        lines =
            String.lines content |> Debug.log "RAW LINES" |> List.map (\s -> s ++ "\n") |> Debug.log "INPUT LINES"

        tokenLoc =
            { begin = begin, end = end }

        meta =
            Meta.make 0 tokenLoc lines 0 "1.2"

        str =
            Meta.stringAtLoc meta.loc lines

        input =
            String.slice begin (end + 1) content
    in
    { accept = input == str, input = input, output = str, meta = meta }


loc i j =
    { begin = i, end = j }


suiteMeta : Test
suiteMeta =
    describe "recovering a substring of the source text from metadata"
        [ test "(1) metaDataTest 1 6" <|
            \_ ->
                metaDataTest 1 6 "abcd\nefgh\nijkl"
                    |> .accept
                    |> Expect.equal True
        , test "(2) metaDataTest 1 6" <|
            \_ ->
                metaDataTest 1 6 "abcd\nefgh\nijkl"
                    |> .output
                    |> Expect.equal "bcd\nef"
        , test "(3) metaDataTest 1 6" <|
            \_ ->
                metaDataTest 1 6 "abcd\nefgh\nijkl"
                    |> (.meta >> .loc)
                    |> Expect.equal { begin = { row = 0, col = 1 }, end = { row = 1, col = 1 } }
        , Test.only <|
            test "(4) metaDataTest 1 6" <|
                \_ ->
                    metaDataTest 0 0 "A\nB\nC\nD\n"
                        |> .accept
                        |> Expect.equal True
        ]
