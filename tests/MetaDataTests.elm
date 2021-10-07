module MetaDataTests exposing (suiteMeta)

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
            Meta.make Meta.getBlockData1 0 tokenLoc lines 0 "1.2"

        str =
            Meta.stringAtLoc meta.loc lines

        input =
            String.slice begin (end + 1) content
    in
    { accept = input == str, input = input, output = str, meta = meta |> Debug.log "META" }


suiteMeta : Test
suiteMeta =
    describe
        "recovering a substring of the source text from metadata"
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
        , test "(4) metaDataTest 1 6" <|
            \_ ->
                metaDataTest 0 0 "A\nB\nC\nD"
                    |> .accept
                    |> Expect.equal True
        , test "(5) metaDataTest 0 2" <|
            \_ ->
                metaDataTest 0 2 "A\nB\nC\nD\n"
                    |> .accept
                    |> Expect.equal True
        , test "(6) metaDataTest 0 3" <|
            \_ ->
                metaDataTest 0 3 "A\nB\nC\nD\n"
                    |> .accept
                    |> Expect.equal True
        , test "(7) metaDataTest 0 6" <|
            \_ ->
                metaDataTest 0 3 "A\nB\nC\nD\n"
                    |> .accept
                    |> Expect.equal True
        , test "(8) metaDataTest 2 6" <|
            \_ ->
                metaDataTest 2 6 "A\nB\nC\nD\n"
                    |> .accept
                    |> Expect.equal True
        , test "(8) metaDataTest 2 5" <|
            \_ ->
                metaDataTest 2 5 "A\nBBBB\nCCCC\nD\n"
                    |> .accept
                    |> Expect.equal True
        , test "(8) metaDataTest 4 9" <|
            \_ ->
                metaDataTest 4 9 "A\nBBBB\nCCCC\nD\n"
                    |> .accept
                    |> Expect.equal True
        ]
