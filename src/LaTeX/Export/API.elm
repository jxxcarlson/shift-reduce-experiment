module LaTeX.Export.API exposing (export, prepareForExportWithImages)

import Block.Block exposing (Block(..), ExprM(..))
import Expression.ASTTools
import LaTeX.Export.Block
import LaTeX.Export.Markdown
import Lang.Lang exposing (Lang(..))
import Markup.API
import Markup.Meta as Meta
import Maybe.Extra


ifApply : Bool -> (a -> a) -> a -> a
ifApply goAhead f a =
    if goAhead then
        f a

    else
        a


export : Lang -> String -> String
export language sourceText =
    let
        _ =
            Debug.log "EXPORT" 1

        ast =
            sourceText
                |> String.lines
                |> Markup.API.parse language 0
                |> .ast
                |> ifApply (language == Markdown) LaTeX.Export.Markdown.normalize

        titleString =
            Expression.ASTTools.getTitle ast |> Maybe.withDefault "Untitled"
    in
    ast |> LaTeX.Export.Block.render titleString


prepareForExportWithImages : Lang -> String -> { source : String, imageUrls : List String }
prepareForExportWithImages language sourceText =
    let
        _ =
            Debug.log "EXPORT" 2

        ast =
            sourceText
                |> String.lines
                |> Markup.API.parse language 0
                |> .ast
                |> ifApply (language == Markdown) LaTeX.Export.Markdown.normalize

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
