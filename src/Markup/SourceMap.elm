module Markup.SourceMap exposing (find, getFragment)

import Markup.Block as Block exposing (Block(..), ExprM(..), Meta)
import Markup.Meta as Meta exposing (ExpressionMeta)


getFragment1 : String -> List String -> List Block -> Maybe String
getFragment1 id sourceText blocks =
    case find id blocks of
        Nothing ->
            Nothing

        Just expressionMeta ->
            Just (Meta.stringAtLoc expressionMeta.loc sourceText)


getFragment : String -> List String -> List Block -> Maybe { fragment : String, meta : ExpressionMeta }
getFragment id sourceText blocks =
    case find id blocks of
        Nothing ->
            Nothing

        Just expressionMeta ->
            Just { fragment = Meta.stringAtLoc expressionMeta.loc sourceText, meta = expressionMeta }


find : String -> List Block -> Maybe ExpressionMeta
find id blocks =
    List.head (List.map (findInBlock id) blocks |> List.concat)


findInBlock : String -> Block -> List ExpressionMeta
findInBlock id block =
    case block of
        Paragraph list meta ->
            List.map (findInExprM id) list |> List.concat

        VerbatimBlock _ _ exprMeta _ ->
            if id == exprMeta.id then
                [ exprMeta ]

            else
                []

        Block _ list _ ->
            List.map (findInBlock id) list |> List.concat

        BError _ ->
            []


findInExprM : String -> ExprM -> List ExpressionMeta
findInExprM id expr =
    List.filter (\e -> e.id == id) (getExprMeta expr)


getExprMeta : ExprM -> List ExpressionMeta
getExprMeta expr =
    case expr of
        TextM _ exprMeta ->
            [ exprMeta ]

        VerbatimM _ _ exprMeta ->
            [ exprMeta ]

        ExprM _ exprMList exprMeta ->
            exprMeta :: (List.map getExprMeta exprMList |> List.concat)
