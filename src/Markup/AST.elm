module Markup.AST exposing (..)

import List.Extra
import Markup.Token as Token


type Expr
    = Text String
    | Verbatim String String
    | Expr String (List Expr)


type alias BlockData =
    { content : List String
    , blockFirstLine : Int
    , id : String
    }


test begin end content =
    let
        block =
            { id = "0.1", blockFirstLine = 0, content = String.lines content }

        block2 =
            augment block

        tokenLoc =
            { begin = begin, end = end }
    in
    makeMeta 0 tokenLoc block2


type alias BlockData2 =
    { content : List String
    , blockFirstLine : Int
    , id : String
    , index : List Int
    , cumulativeLengths : List Int
    }


augment : BlockData -> BlockData2
augment blockData =
    { content = blockData.content |> List.map (\s -> s ++ "\n")
    , blockFirstLine = blockData.blockFirstLine
    , id = blockData.id
    , index = linePositions blockData.content
    , cumulativeLengths =
        List.map (String.length >> (\n -> n + 1)) blockData.content
            |> List.Extra.scanl (+) 0
            |> List.drop 1
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


makeMeta : Int -> Token.Loc -> BlockData2 -> Meta
makeMeta count tokenLoc blockData =
    let
        n1 =
            getLineNumber tokenLoc.begin blockData.index

        n2 =
            getLineNumber tokenLoc.end blockData.index

        p1 =
            List.take n1 blockData.cumulativeLengths |> List.head |> Maybe.withDefault 0

        p2 =
            List.take n2 blockData.cumulativeLengths |> List.head |> Maybe.withDefault 0

        first =
            { row = n1, col = tokenLoc.begin - p1 }

        last =
            { row = n2, col = tokenLoc.end - p2 }

        loc =
            { begin = first, end = last }
    in
    { id = blockData.id ++ "." ++ String.fromInt count
    , loc = loc
    }


type alias Meta =
    { id : String
    , loc : Loc
    }


type alias Position =
    { row : Int, col : Int }


type alias Loc =
    { begin : Position, end : Position }
