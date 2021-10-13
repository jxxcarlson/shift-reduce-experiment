module Render.AST2 exposing (getName, stringValueOfBlockList, stringValueOfList, textToString)

import Block.Block exposing (Block(..), ExprM(..))


getName : ExprM -> Maybe String
getName text =
    case text of
        ExprM str _ _ ->
            Just str

        _ ->
            Nothing


stringValueOfBlockList : List Block -> String
stringValueOfBlockList blocks =
    List.map stringValueOfBlock blocks |> String.join "\n"


stringValueOfBlock : Block -> String
stringValueOfBlock block =
    case block of
        Paragraph exprMList _ ->
            stringValueOfList exprMList

        VerbatimBlock _ strings _ _ ->
            String.join "\n" strings

        Block _ blocks _ ->
            List.map stringValueOfBlock blocks |> String.join "\n"

        BError str ->
            str


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
