module Markup.ASTTools exposing (filter, getHeadings, getText, getTitle, listExprMToString)

import Markup.AST
import Markup.Block exposing (Block(..), ExprM(..))


getText : ExprM -> Maybe String
getText text =
    case text of
        TextM str _ ->
            Just str

        VerbatimM _ str _ ->
            Just (String.replace "`" "" str)

        _ ->
            Nothing


getName : ExprM -> Maybe String
getName text =
    case text of
        ExprM str _ _ ->
            Just str

        _ ->
            Nothing


listExprMToString : List ExprM -> String
listExprMToString list =
    List.map exprMToString list |> String.join "\n"


exprMToString : ExprM -> String
exprMToString text =
    case text of
        TextM string _ ->
            string

        ExprM _ textList _ ->
            List.map exprMToString textList |> String.join "\n"

        ArgM textList _ ->
            List.map exprMToString textList |> String.join "\n"

        VerbatimM _ str _ ->
            str


getTitle : List Block -> Maybe String
getTitle blocks =
    filterStrict "title" blocks |> List.head |> Maybe.map (exprMToString >> String.trim)


getHeadings : List Block -> List ExprM
getHeadings blocks =
    filter "heading" blocks


filter : String -> List Block -> List ExprM
filter key blocks =
    List.map (filter_ key) blocks |> List.concat


filterStrict : String -> List Block -> List ExprM
filterStrict key blocks =
    List.map (filterStrict_ key) blocks |> List.concat


filterStrictNot : String -> List Block -> List ExprM
filterStrictNot key blocks =
    List.map (filterStrictNot_ key) blocks |> List.concat


filter_ : String -> Block -> List ExprM
filter_ key block =
    case block of
        Paragraph textList _ ->
            List.filter (\t -> Maybe.map (String.contains key) (getName t) == Just True) textList

        Block _ blocks _ ->
            List.map (filter_ key) blocks |> List.concat

        _ ->
            []


filterStrict_ : String -> Block -> List ExprM
filterStrict_ key block =
    case block of
        Paragraph textList _ ->
            List.filter (\t -> Just key == getName t) textList

        Block _ blocks _ ->
            List.map (filterStrict_ key) blocks |> List.concat

        _ ->
            []


filterStrictNot_ : String -> Block -> List ExprM
filterStrictNot_ key block =
    case block of
        Paragraph textList _ ->
            List.filter (\t -> Just key /= getName t) textList

        Block _ blocks _ ->
            List.map (filterStrict_ key) blocks |> List.concat

        _ ->
            []
