module Render.AST2 exposing (getName, stringValueOfList, textToString)

import Markup.Block exposing (ExprM(..))
import Markup.Token as Token


getName : ExprM -> Maybe String
getName text =
    case text of
        ExprM str _ _ ->
            Just str

        _ ->
            Nothing


stringValueOfList : List ExprM -> String
stringValueOfList textList =
    String.join " " (List.map stringValue textList)


stringValue : ExprM -> String
stringValue text =
    case text of
        TextM str _ ->
            str

        ExprM _ textList _ ->
            String.join " " (List.map stringValue textList)

        ArgM textList _ ->
            String.join " " (List.map stringValue textList)

        VerbatimM _ str _ ->
            str


textToString : ExprM -> String
textToString text =
    case text of
        TextM string _ ->
            string

        ExprM _ textList _ ->
            List.map textToString textList |> String.join "\n"

        ArgM textList _ ->
            List.map textToString textList |> String.join "\n"

        VerbatimM _ str _ ->
            str
