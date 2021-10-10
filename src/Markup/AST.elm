module Markup.AST exposing (Expr(..), args2M, getName, stringValueOfList, textToString)

import Markup.Token as Token
import Maybe.Extra


type Expr
    = Text String Token.Loc
    | Verbatim String String Token.Loc
    | Arg (List Expr) Token.Loc
    | Expr String (List Expr) Token.Loc


dummy =
    { begin = 0, end = 0 }


{-| [Text "a b c d"] -> [Text "a b c", Text "d"]
-}
args2M : List Expr -> List Expr
args2M exprList =
    let
        args =
            args2 exprList
    in
    [ Text args.first dummy, Text args.last dummy ]


args2 : List Expr -> { first : String, last : String }
args2 exprList =
    let
        args =
            exprListToStringList exprList |> String.join " " |> String.words

        n =
            List.length args

        first =
            List.take (n - 1) args |> String.join " "

        last =
            List.drop (n - 1) args |> String.join " "
    in
    { first = first, last = last }


exprListToStringList : List Expr -> List String
exprListToStringList exprList =
    List.map getText exprList
        |> Maybe.Extra.values
        |> List.map String.trim
        |> List.filter (\s -> s /= "")


getText : Expr -> Maybe String
getText text =
    case text of
        Text str _ ->
            Just str

        Verbatim _ str _ ->
            Just (String.replace "`" "" str)

        _ ->
            Nothing


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
