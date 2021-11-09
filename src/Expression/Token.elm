module Expression.Token exposing
    ( Loc
    , Token(..)
    , TokenStack
    , TokenSymbol(..)
    , dummyLoc
    , isSymbol
    , length
    , push
    , reduce
    , startPositionOf
    , stringValue
    , symbolToString
    )

import Expression.Error exposing (ErrorData)


type Token
    = Text String Loc
    | Verbatim String String Loc
    | Symbol String Loc
    | FunctionName String Loc
    | MarkedText String String Loc
    | AnnotatedText String String String Loc
    | Special String String Loc -- eg, for @title[The Greatest Book Ever!]
    | TokenError ErrorData Loc


symbolToString : Token -> Maybe String
symbolToString token =
    case token of
        Symbol str _ ->
            Just str

        _ ->
            Nothing


stringValue : Token -> String
stringValue token =
    case token of
        Text str _ ->
            str

        Verbatim _ str _ ->
            str

        Symbol str _ ->
            str

        FunctionName str _ ->
            str

        MarkedText _ str _ ->
            str

        AnnotatedText _ _ str _ ->
            str

        Special _ str _ ->
            str

        TokenError _ _ ->
            "token error"


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

        AnnotatedText _ _ _ loc ->
            loc.begin

        Special _ _ loc ->
            loc.begin

        TokenError _ loc ->
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

        AnnotatedText _ _ _ loc ->
            loc.end - loc.begin

        Special _ _ loc ->
            loc.end - loc.begin

        TokenError _ loc ->
            loc.end - loc.begin


type TokenSymbol
    = LBR
    | RBR
    | LPAREN
    | RPAREN


reduce : TokenStack -> TokenStack
reduce stack =
    stack |> List.reverse |> reduce_ |> List.reverse


reduce_ : TokenStack -> TokenStack
reduce_ stack =
    case stack of
        LBR :: RBR :: LPAREN :: RPAREN :: [] ->
            []

        LBR :: RBR :: LPAREN :: RPAREN :: rest ->
            reduce_ (LBR :: RBR :: rest)

        LBR :: RBR :: LPAREN :: rest ->
            case List.reverse rest of
                RPAREN :: rest2 ->
                    reduce_ (List.reverse rest2)

                _ ->
                    stack

        LPAREN :: rest ->
            case List.reverse rest of
                RPAREN :: rest2 ->
                    reduce_ (List.reverse rest2)

                _ ->
                    stack

        _ ->
            stack


type alias TokenStack =
    List TokenSymbol


push : Token -> TokenStack -> TokenStack
push token stack =
    case token of
        Symbol "[" _ ->
            LBR :: stack

        Symbol "]" _ ->
            RBR :: stack

        Symbol "(" _ ->
            LPAREN :: stack

        Symbol ")" _ ->
            RPAREN :: stack

        _ ->
            stack


{-|

    Grammar: S -> [ ] A ; A -> ( ) | ( ) A | | ( S )

-}
