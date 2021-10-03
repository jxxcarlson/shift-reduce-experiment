module Token exposing
    ( Loc
    , Token(..)
    , content
    , length
    , startPositionOf
    )


type Token
    = Text String Loc
    | Verbatim String String Loc
    | Symbol String Loc


type alias Loc =
    { begin : Int, end : Int }


startPositionOf : Token -> Int
startPositionOf token =
    case token of
        Text _ loc ->
            loc.begin

        Verbatim _ _ loc ->
            loc.begin

        Symbol _ loc ->
            loc.begin


content : Token -> String
content token =
    case token of
        Text str _ ->
            str

        Verbatim _ str _ ->
            str

        Symbol str _ ->
            str


length : Token -> Int
length token =
    String.length (content token)
