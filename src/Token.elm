module Token exposing (Loc, Token(..))


type Token
    = Text String Loc
    | Verbatim String String Loc
    | Symbol String Loc


type alias Loc =
    { begin : Int, end : Int }
