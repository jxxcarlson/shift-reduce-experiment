module Markup.AST exposing
    ( BlockData
    , BlockData2
    , Expr(..)
    , Loc
    , Meta
    , Position
    , augment
    , makeMeta
    , test
    )

import List.Extra
import Markup.Token as Token


type Expr
    = Text String Token.Loc
    | Verbatim String String Token.Loc
    | Expr String (List Expr) Token.Loc


type alias BlockData =
    { content : List String
    , blockFirstLine : Int
    , id : String
    }


type alias BlockData2 =
    { content : List String
    , blockFirstLine : Int
    , id : String
    , index : List Int
    , cumulativeLengths : List Int
    }


type alias Meta =
    { id : String
    , loc : Loc
    }


type alias Position =
    { row : Int, col : Int }


type alias Loc =
    { begin : Position, end : Position }


{-|

    Construct a Meta value given
        - the block count
        - a location (tokenLoc: Token.Loc)
        - blockData

-}
makeMeta : Int -> Token.Loc -> BlockData -> Meta
makeMeta count tokenLoc blockData =
    let
        blockData2 =
            augment blockData

        n1 =
            getLineNumber tokenLoc.begin blockData2.index

        n2 =
            getLineNumber tokenLoc.end blockData2.index

        p1 =
            List.take n1 blockData2.cumulativeLengths |> List.head |> Maybe.withDefault 0

        p2 =
            List.take n2 blockData2.cumulativeLengths |> List.head |> Maybe.withDefault 0

        first =
            { row = n1, col = tokenLoc.begin - p1 }

        last =
            { row = n2, col = tokenLoc.end - p2 }

        loc =
            { begin = first, end = last }
    in
    { id = blockData2.id ++ "." ++ String.fromInt count
    , loc = loc
    }


test : Int -> Int -> String -> { accept : Bool, input : String, output : String, zmeta : Meta }
test begin end content =
    let
        block =
            { id = "0.1", blockFirstLine = 0, content = String.lines content }

        block2 =
            augment block

        tokenLoc =
            { begin = begin, end = end }

        meta =
            makeMeta 0 tokenLoc block

        str =
            stringAtLoc meta.loc block2.content

        input =
            String.slice begin (end + 1) content
    in
    { accept = input == str, input = input, output = str, zmeta = meta }


stringAtLoc : Loc -> List String -> String
stringAtLoc loc inputLines =
    let
        selectedLines =
            List.filter (\( i, _ ) -> i >= loc.begin.row && i <= loc.end.row) (List.indexedMap (\i s -> ( i, s )) inputLines) |> Debug.log "SELECTED LINES"

        n =
            List.length selectedLines

        transform ( i, line ) =
            if i == loc.begin.row && i /= loc.end.row then
                String.dropLeft loc.begin.col line

            else if i == loc.begin.row && i == loc.end.row then
                String.dropLeft loc.begin.col line |> String.dropRight (String.length line - loc.end.col - 1 |> Debug.log "INDEX FOR DROP RIGHT")

            else if i == loc.end.row then
                String.dropRight (String.length line - loc.end.col - 1 |> Debug.log "INDEX FOR DROP RIGHT") line

            else
                line
    in
    selectedLines |> List.map transform |> String.join "" |> Debug.log "stringAtLoc, OUT"


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
