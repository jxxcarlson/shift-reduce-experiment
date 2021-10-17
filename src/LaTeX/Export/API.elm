module LaTeX.Export.API exposing (..)

import Expression.ASTTools
import LaTeX.Export.Block
import Lang.Lang exposing (Lang)
import Markup.API
import Maybe.Extra


export : Lang -> String -> String
export language sourceText =
    let
        ast =
            sourceText
                |> String.lines
                |> Markup.API.parse language 0
                |> .ast

        titleString =
            Expression.ASTTools.getTitle ast |> Maybe.withDefault "Untitled"
    in
    ast |> LaTeX.Export.Block.render titleString


imageUrlList ast =
    ast
        |> List.map (macroValue_ "image")
        |> Maybe.Extra.values



--macroValue_ : String -> List LatexExpression -> Maybe String
--macroValue_ macroName list =
--    list
--        |> filterMacro macroName
--        |> List.head
--        |> Maybe.map getMacroArgs2
--        |> Maybe.andThen List.head
--        |> Maybe.andThen List.head
--        |> Maybe.map getString
--
--
--getMacroArgs2 : LatexExpression -> List (List LatexExpression)
--getMacroArgs2 latexExpression =
--    case latexExpression of
--        Macro name optArgs args ->
--            args
--                |> List.map latexList2List
--
--        _ ->
--            []
