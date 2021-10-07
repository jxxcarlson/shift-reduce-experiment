module Markup.AST exposing (Expr(..), getName, stringValueOfList, textToString)

import Markup.Token as Token


type Expr
    = Text String Token.Loc
    | Verbatim String String Token.Loc
    | Arg (List Expr) Token.Loc
    | Expr String (List Expr) Token.Loc


getName : Expr -> Maybe String
getName text =
    case text of
        Expr str _ _ ->
            Just str

        _ ->
            Nothing


stringValueOfList : List Expr -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : Expr -> String
stringValue text =
    case text of
        Text str _ ->
            str

        Expr _ textList _ ->
            String.join " " (List.map stringValue textList)

        Arg textList _ ->
            String.join " " (List.map stringValue textList)

        Verbatim _ str _ ->
            str


textToString : Expr -> String
textToString text =
    case text of
        Text string _ ->
            string

        Expr _ textList _ ->
            List.map textToString textList |> String.join "\n"

        Arg textList _ ->
            List.map textToString textList |> String.join "\n"

        Verbatim _ str _ ->
            str
