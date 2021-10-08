module Markup.ASTTools exposing (FilterType(..), filter, filterBlockByName, filterStrictBlock, getHeadings, getText, getTitle, listExprMToString)

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
    let
        result =
            filterStrictBlock Equality "title" blocks
    in
    if result == "" then
        Nothing

    else
        Just result


getHeadings : List Block -> List ExprM
getHeadings blocks =
    filter Contains "heading" blocks


filter : FilterType -> String -> List Block -> List ExprM
filter filterType key blocks =
    List.map (filter_ filterType key) blocks |> List.concat


filterStrict : String -> List Block -> List ExprM
filterStrict key blocks =
    List.map (filterStrict_ key) blocks |> List.concat


filterStrictBlock : FilterType -> String -> List Block -> String
filterStrictBlock filterType key blocks =
    List.map (filterStrictBlock_ filterType key) blocks |> String.join ""


filterStrictNot : String -> List Block -> List ExprM
filterStrictNot key blocks =
    List.map (filterStrictNot_ key) blocks |> List.concat


filter_ : FilterType -> String -> Block -> List ExprM
filter_ filterType key block =
    case block of
        Paragraph textList _ ->
            case filterType of
                Equality ->
                    List.filter (\t -> Maybe.map (\x -> x == key) (getName t) == Just True) textList

                Contains ->
                    List.filter (\t -> Maybe.map (String.contains key) (getName t) == Just True) textList

        Block name blocks _ ->
            case filterType of
                Equality ->
                    if key == name then
                        List.map extractContents blocks |> List.concat

                    else
                        []

                Contains ->
                    if String.contains key name then
                        List.map extractContents blocks |> List.concat

                    else
                        []

        _ ->
            []


extractContents : Block -> List ExprM
extractContents block =
    case block of
        Paragraph contents _ ->
            contents

        _ ->
            []


type FilterType
    = Equality
    | Contains


filterStrictBlock_ : FilterType -> String -> Block -> String
filterStrictBlock_ filterType key block =
    case block of
        Paragraph textList _ ->
            case filterType of
                Equality ->
                    List.filter (\t -> Just key == getName t) textList |> Debug.log "(1)" |> listExprMToString

                Contains ->
                    List.filter (\t -> Maybe.map2 String.contains (Just key) (getName t) == Just True) textList |> Debug.log "(1)" |> listExprMToString

        Block name blocks _ ->
            case filterType of
                Equality ->
                    if key == name then
                        List.map stringContentOfNamedBlock blocks |> String.join ""

                    else
                        ""

                Contains ->
                    if String.contains key name then
                        List.map stringContentOfNamedBlock blocks |> String.join ""

                    else
                        ""

        _ ->
            ""


filterBlockByName : String -> Block -> String
filterBlockByName key block =
    case block of
        Block name blocks _ ->
            if key == name then
                List.map stringContentOfNamedBlock blocks |> String.join ""

            else
                ""

        _ ->
            ""


stringContentOfNamedBlock : Block -> String
stringContentOfNamedBlock block =
    case block of
        Paragraph exprMList _ ->
            listExprMToString exprMList

        VerbatimBlock _ strings _ _ ->
            String.join "\n" strings

        Block _ blocks _ ->
            List.map stringContentOfNamedBlock blocks |> String.join "\n"

        BError str ->
            str


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
