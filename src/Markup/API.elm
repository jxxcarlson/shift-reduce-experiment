module Markup.API exposing (..)

import Block.Parser
import Block.State
import Element as E exposing (Element)
import Element.Font as Font
import Markup.ASTTools as ASTTools
import Markup.Block as Block exposing (Block)
import Markup.Lang exposing (Lang(..))
import Markup.Markup as Markup
import Markup.Simplify as Simplify
import Render.Block
import Render.Settings
import Render.Text
import Utility


defaultSettings : Render.Settings.Settings
defaultSettings =
    { width = 500, titleSize = 30, showTOC = True }


p : Lang -> String -> List Simplify.BlockS
p lang str =
    parse lang 0 (String.lines str) |> .ast |> Simplify.blocks


rl : String -> List (Element msg)
rl str =
    renderFancy { width = 500, titleSize = 30, showTOC = True } L1 0 (String.lines str)


parse : Lang -> Int -> List String -> { ast : List Block, accumulator : Block.State.Accumulator }
parse lang generation lines =
    let
        state =
            Block.Parser.run lang generation lines
    in
    { ast = List.map (Block.map (Markup.parseExpr lang)) state.committed, accumulator = state.accumulator }


{-| -}
getTitle : List Block -> Maybe String
getTitle =
    ASTTools.getTitle


renderFancy : Render.Settings.Settings -> Lang -> Int -> List String -> List (Element msg)
renderFancy settings language count source =
    let
        parseData =
            parse language count source

        ast =
            parseData.ast

        toc_ : List (Element msg)
        toc_ =
            tableOfContents count { width = 500 } parseData.accumulator ast

        maybeTitleString =
            ASTTools.getTitle ast

        docTitle =
            case maybeTitleString of
                Nothing ->
                    E.none

                Just titleString ->
                    E.el [ Font.size settings.titleSize, Utility.elementAttribute "id" "title" ] (E.text (titleString |> String.replace "\n" " "))

        toc =
            if List.length toc_ > 1 then
                E.column [ E.paddingXY 0 24, E.spacing 8 ] toc_

            else
                E.none

        renderedText_ : List (Element msg)
        renderedText_ =
            render count settings parseData.accumulator ast
    in
    if settings.showTOC then
        docTitle :: toc :: renderedText_

    else
        docTitle :: renderedText_


tableOfContents : Int -> Settings -> Block.State.Accumulator -> List Block -> List (Element msg)
tableOfContents generation settings accumulator blocks =
    blocks |> ASTTools.getHeadings |> Render.Text.viewTOC generation defaultSettings accumulator


{-| -}
compile : Lang -> Int -> Render.Settings.Settings -> List String -> List (Element msg)
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
    { width : Int }


prepareForExport : String -> ( List String, String )
prepareForExport str =
    ( [ "image urls" ], "document content" )
