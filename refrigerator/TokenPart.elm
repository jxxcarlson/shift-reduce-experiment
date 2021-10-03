module TokenPart exposing (TokenPart(..))


type TokenPart
    = AcceptChar Char
    | RejectCharList (List Char)
    | RejectChar Char
    | AcceptCharList (List Char)
