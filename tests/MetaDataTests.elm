module MetaDataTests exposing (..)

import Expect
import Markup.Meta as Meta
import Test exposing (..)


blockTest : Int -> Int -> String -> { accept : Bool, input : String, output : String, meta : Meta.ExpressionMeta }
blockTest begin end content =
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


suiteMarkdown : Test
suiteMarkdown =
    Test.only <|
        describe "recovering a substring of the source text from metadata"
            [ test "(1)lockTest 1 6" <|
                \_ ->
                    blockTest 1 6 "abcd\nefgh\nijkl"
                        |> .accept
                        |> Expect.equal True
            , test "(2) blockTest 1 6" <|
                \_ ->
                    blockTest 1 6 "abcd\nefgh\nijkl"
                        |> .output
                        |> Expect.equal "bcd\nef"
            , test "(3) blockTest 1 6" <|
                \_ ->
                    blockTest 1 6 "abcd\nefgh\nijkl"
                        |> (.meta >> .loc)
                        |> Expect.equal { begin = { row = 0, col = 1 }, end = { row = 1, col = 1 } }
            ]
