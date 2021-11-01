module Markup.API exposing
    ( Settings
    , compile
    , defaultSettings
    , p
    , parse
    , prepareForExport
    , render
    , renderFancy
    , rl
    , tableOfContents
    )

import Block.Block exposing (Block)
import Block.BlockTools
import Block.Function
import Block.Parser
import Block.State
import Element as E exposing (Element)
import Element.Font as Font
import Expression.ASTTools as ASTTools
import Expression.Parser
import LaTeX.Export.Markdown
import Lang.Lang exposing (Lang(..))
import Markup.Simplify as Simplify
import Render.Block
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Render.Text
import Utility


defaultSettings : Settings
defaultSettings =
    { width = 500
    , titleSize = 30
    , paragraphSpacing = 28
    , showTOC = True
    , showErrorMessages = False
    }


p : Lang -> String -> List Simplify.BlockS
p lang str =
    parse lang 0 (String.lines str) |> .ast |> Simplify.blocks


rl : String -> List (Element MarkupMsg)
rl str =
    renderFancy defaultSettings L1 0 (String.lines str)



-- NOTE THE AST TRANSFORMATION BELOW


parse : Lang -> Int -> List String -> { ast : List Block, accumulator : Block.State.Accumulator }
parse lang generation lines =
    let
        state =
            Block.Parser.run lang generation lines

        ast =
            case lang of
                Markdown ->
                    List.map (Block.BlockTools.map (Expression.Parser.parseExpr lang) >> Block.Function.fixMarkdownBlock) state.committed
                        |> LaTeX.Export.Markdown.putListItemsAsChildrenOfBlock

                _ ->
                    List.map (Block.BlockTools.map (Expression.Parser.parseExpr lang) >> Block.Function.fixMarkdownBlock) state.committed
    in
    { ast = ast
    , accumulator = state.accumulator
    }



{-

   1. List.map sblockToBlock state.committed

   2. List.foldl folder initialData state.committed

      initialData = {accumulator = Accumulator.empty, blocks = [] }

      folder : SBlock -> {accumulator : Accumulator, blocks : List Block} -> {accumulator : Accumulator, blocks : List Block}
      folder sblock acc =
        let
          block = sblockToBlock sblock
        in
          {accumulator = updateAccumulatorWithBlock bloc acc.accumulator, blocks = block::acc.blocks}


-}


renderFancy : Render.Settings.Settings -> Lang -> Int -> List String -> List (Element MarkupMsg)
renderFancy settings language count source =
    let
        parseData =
            parse language count source

        ast =
            parseData.ast

        toc_ : List (Element MarkupMsg)
        toc_ =
            tableOfContents count settings parseData.accumulator ast

        maybeTitleString =
            ASTTools.getItem "title" ast

        maybeAuthorString =
            ASTTools.getItem "author" ast

        maybeDateString =
            ASTTools.getItem "date" ast

        docTitle =
            case maybeTitleString of
                Nothing ->
                    E.none

                Just titleString ->
                    E.el [ Font.size settings.titleSize, Utility.elementAttribute "id" "title" ] (E.text (titleString |> String.replace "\n" " "))

        author =
            case maybeAuthorString of
                Nothing ->
                    E.none

                Just authorString ->
                    let
                        size =
                            round (toFloat settings.titleSize / 1.4)
                    in
                    E.el [ Font.size size, Utility.elementAttribute "id" "author" ] (E.text (authorString |> String.replace "\n" " "))

        date =
            case maybeDateString of
                Nothing ->
                    E.none

                Just dateString ->
                    let
                        size =
                            round (toFloat settings.titleSize / 1.8)
                    in
                    E.el [ Font.size size, Utility.elementAttribute "id" "date" ] (E.text (dateString |> String.replace "\n" " "))

        toc =
            if List.length toc_ > 1 then
                E.column [ E.paddingXY 0 24, E.spacing 8 ] toc_

            else
                E.none

        renderedText_ : List (Element MarkupMsg)
        renderedText_ =
            render count settings parseData.accumulator ast
    in
    if settings.showTOC then
        docTitle :: author :: date :: toc :: renderedText_

    else
        docTitle :: author :: date :: renderedText_


tableOfContents : Int -> Settings -> Block.State.Accumulator -> List Block -> List (Element MarkupMsg)
tableOfContents generation settings accumulator blocks =
    blocks |> ASTTools.getHeadings |> Render.Text.viewTOC generation defaultSettings accumulator


{-| -}
compile : Lang -> Int -> Render.Settings.Settings -> List String -> List (Element MarkupMsg)
compile language generation settings lines =
    let
        parseData =
            parse language generation lines
    in
    parseData.ast |> Render.Block.render generation settings parseData.accumulator


render =
    Render.Block.render


{-| -}
type alias Settings =
    Render.Settings.Settings


prepareForExport : String -> ( List String, String )
prepareForExport str =
    ( [ "image urls" ], "document content" )
