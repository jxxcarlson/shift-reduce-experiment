module Markup.Meta exposing (..)

import List.Extra
import Markup.Token as Token


type alias ExpressionMeta =
    { id : String
    , loc : { begin : { row : Int, col : Int }, end : { row : Int, col : Int } }
    }


type alias BlockData =
    { lines : List String
    , content : String
    , firstLine : Int
    , id : String
    , index : List Int
    , cumulativeLengths : List Int
    }


type alias Loc =
    { begin : Position, end : Position }


type alias Position =
    { row : Int, col : Int }


metaDataTest : Int -> Int -> String -> { accept : Bool, input : String, output : String, meta : ExpressionMeta }
metaDataTest begin end content =
    let
        lines =
            String.lines content |> Debug.log "RAW LINES" |> List.map (\s -> s ++ "\n") |> Debug.log "INPUT LINES"

        tokenLoc =
            { begin = begin, end = end }

        meta =
            make 0 tokenLoc lines 0 "1.2"

        str =
            stringAtLoc meta.loc lines

        input =
            String.slice begin (end + 1) content
    in
    { accept = input == str, input = input, output = str, meta = meta |> Debug.log "META" }


{-|

    Given a location = { begin = {row :Int, col: Int}, end = {row: Int, col : Int}
    and list of strings, return the substring specified by that location information.

-}
stringAtLoc : Loc -> List String -> String
stringAtLoc loc inputLines =
    let
        selectedLines =
            List.filter (\( i, _ ) -> i >= loc.begin.row && i <= loc.end.row) (List.indexedMap (\i s -> ( i, s )) inputLines) |> Debug.log "SELECTED LINES"

        n =
            List.length selectedLines

        take k str =
            -- TODO: better implementation
            String.slice 0 (k + 1) str

        transform ( i, line ) =
            if i == loc.begin.row && i /= loc.end.row then
                String.dropLeft loc.begin.col line |> Debug.log "T, 1"

            else if i == loc.begin.row && i == loc.end.row then
                String.dropLeft loc.begin.col (take loc.end.col line) |> Debug.log "T, 2"

            else if i == loc.end.row then
                take loc.end.col line |> Debug.log "T, 3"

            else
                line
    in
    selectedLines |> List.map transform |> String.join "" |> Debug.log "stringAtLoc, OUT"


{-|

    Construct a Meta value given
        - the block count
        - a location (tokenLoc: Token.Loc)
        - blockData

-}
make : Int -> Token.Loc -> List String -> Int -> String -> ExpressionMeta
make count tokenLoc lines blockFirstLine id =
    let
        blockData =
            getBlockData lines blockFirstLine id

        n1 =
            getLineNumber tokenLoc.begin blockData.index |> Debug.log "ROW 1"

        n2 =
            getLineNumber tokenLoc.end blockData.index |> Debug.log "ROW 2"

        p1 =
            List.drop n1 blockData.cumulativeLengths |> List.head |> Maybe.withDefault 0 |> Debug.log "P1"

        p2 =
            List.drop n2 blockData.cumulativeLengths |> List.head |> Maybe.withDefault 0 |> Debug.log "P2"

        c1 =
            tokenLoc.begin - p1 |> Debug.log "COL 1"

        c2 =
            tokenLoc.end - p2 |> Debug.log "COL 2"

        first =
            { row = n1, col = c1 }

        last =
            { row = n2, col = c2 }

        loc =
            { begin = first, end = last }
    in
    { id = blockData.id ++ "." ++ String.fromInt count
    , loc = loc
    }


{-|

    Compute BlocData from a list of strings, an integer
    representing the first line of those line in some source text,
    and a string representing an id, compute the BlockData.

    This is data is used by exprToExprM.

    Note: Assume that the strings are not terminated by newlines

-}
getBlockData : List String -> Int -> String -> BlockData
getBlockData lines firstLine id =
    { lines = lines
    , content = lines |> List.map (\line -> line ++ "\n") |> String.join ""
    , firstLine = firstLine
    , id = id
    , index = linePositions lines
    , cumulativeLengths =
        List.map String.length lines
            |> List.Extra.scanl (+) 0
            |> Debug.log "CUMULATIVE LENGTHS"
    }


getLineNumber : Int -> List Int -> Int
getLineNumber pos positions =
    List.filter (\p -> p <= pos) positions |> List.length |> (\n -> n - 1)


linePositions : List String -> List Int
linePositions lines =
    let
        head : List Int -> Int
        head list =
            List.head list |> Maybe.withDefault 0
    in
    List.foldl (\line acc -> (String.length line + head acc) :: acc) [ 0 ] lines
        |> List.drop 1
        |> List.reverse
