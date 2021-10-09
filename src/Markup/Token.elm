module Markup.Token exposing
    ( Loc
    , Token(..)
    , dummyLoc
    , isSymbol
    , length
    , startPositionOf
    )


type Token
    = Text String Loc
    | Verbatim String String Loc
    | Symbol String Loc
    | FunctionName String Loc
    | MarkedText String String Loc


isSymbol : Token -> Bool
isSymbol token =
    case token of
        Symbol _ _ ->
            True

        _ ->
            False


type alias Loc =
    { begin : Int, end : Int }


dummyLoc =
    { begin = 0, end = 0 }


startPositionOf : Token -> Int
startPositionOf token =
    case token of
        Text _ loc ->
            loc.begin

        Verbatim _ _ loc ->
            loc.begin

        Symbol _ loc ->
            loc.begin

        FunctionName _ loc ->
            loc.begin

        MarkedText _ _ loc ->
            loc.begin


length : Token -> Int
length token =
    case token of
        Text _ loc ->
            loc.end - loc.begin

        Verbatim _ _ loc ->
            loc.end - loc.begin

        Symbol _ loc ->
            loc.end - loc.begin

        FunctionName _ loc ->
            loc.end - loc.begin

        MarkedText _ _ loc ->
            loc.end - loc.begin
