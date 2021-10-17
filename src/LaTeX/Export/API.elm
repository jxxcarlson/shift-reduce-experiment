module LaTeX.Export.API exposing (export, parepareForExportWithImages)

import Block.Block exposing (Block(..), ExprM(..))
import Expression.ASTTools
import LaTeX.Export.Block
import Lang.Lang exposing (Lang)
import Markup.API
import Markup.Meta as Meta
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


parepareForExportWithImages : Lang -> String -> { source : String, imageUrls : List String }
parepareForExportWithImages language sourceText =
    let
        ast =
            sourceText
                |> String.lines
                |> Markup.API.parse language 0
                |> .ast

        titleString =
            Expression.ASTTools.getTitle ast |> Maybe.withDefault "Untitled"

        source =
            ast |> LaTeX.Export.Block.render titleString

        imageUrls =
            getImageURLs ast
    in
    { source = source, imageUrls = imageUrls }


getImageURLs : List Block -> List String
getImageURLs blocks =
    Expression.ASTTools.filter Expression.ASTTools.Contains "heading" blocks
        |> List.map Expression.ASTTools.getText
        |> Maybe.Extra.values
