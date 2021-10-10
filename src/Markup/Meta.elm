module Markup.Meta exposing
    ( BlockData
    , ExpressionMeta
    , Loc
    , Position
    , dummy
    , getBlockData
    , getBlockData1
    , make
    , stringAtLoc
    )

import List.Extra
import Markup.Token as Token


type alias ExpressionMeta =
    { id : String
    , loc : { begin : { row : Int, col : Int }, end : { row : Int, col : Int } }
    }


dummy =
    { id = "dummy", loc = { begin = { row = 0, col = 0 }, end = { row = 0, col = 0 } } }


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


{-|

    Given a location = { begin = {row :Int, col: Int}, end = {row: Int, col : Int}
    and list of strings, return the substring specified by that location information.

-}
stringAtLoc : Loc -> List String -> String
stringAtLoc loc inputLines =
    let
        selectedLines =
            List.filter (\( i, _ ) -> i >= loc.begin.row && i <= loc.end.row) (List.indexedMap (\i s -> ( i, s )) inputLines)

        take k str =
            -- TODO: better implementation
            String.slice 0 (k + 1) str

        transform ( i, line ) =
            if i == loc.begin.row && i /= loc.end.row then
                String.dropLeft loc.begin.col line

            else if i == loc.begin.row && i == loc.end.row then
                String.dropLeft loc.begin.col (take loc.end.col line)

            else if i == loc.end.row then
                take loc.end.col line

            else
                line
    in
    selectedLines |> List.map transform |> String.join ""


{-|

    Construct a Meta value given
        - the block count
        - a location (tokenLoc: Token.Loc)
        - blockData

-}
make : (List String -> Int -> String -> BlockData) -> Int -> Token.Loc -> List String -> Int -> String -> ExpressionMeta
make getBlockData_ count tokenLoc lines blockFirstLine id =
    let
        blockData =
            getBlockData_ lines blockFirstLine id

        n1 =
            getLineNumber tokenLoc.begin blockData.index

        n2 =
            getLineNumber tokenLoc.end blockData.index

        p1 =
            List.drop n1 blockData.cumulativeLengths |> List.head |> Maybe.withDefault 0

        p2 =
            List.drop n2 blockData.cumulativeLengths |> List.head |> Maybe.withDefault 0

        c1 =
            tokenLoc.begin - p1

        c2 =
            tokenLoc.end - p2

        first =
            { row = blockFirstLine + n1, col = c1 }

        last =
            { row = blockFirstLine + n2, col = c2 }

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
    let
        terminatedLines =
            lines |> List.map (\line -> line ++ "\n")
    in
    { lines = lines
    , content = terminatedLines |> String.join ""
    , firstLine = firstLine
    , id = id
    , index = linePositions terminatedLines
    , cumulativeLengths =
        List.map String.length terminatedLines
            |> List.Extra.scanl (+) 0
    }


getBlockData1 : List String -> Int -> String -> BlockData
getBlockData1 lines firstLine id =
    { lines = lines
    , content = lines |> String.join "\n"
    , firstLine = firstLine
    , id = id
    , index = linePositions lines
    , cumulativeLengths =
        List.map String.length lines
            |> List.Extra.scanl (+) 0
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
