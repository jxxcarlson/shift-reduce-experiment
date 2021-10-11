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
import Render.Text


p : Lang -> String -> List Simplify.BlockS
p lang str =
    parse lang 0 (String.lines str) |> .ast |> Simplify.blocks


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


renderFancy : Lang -> Int -> List String -> List (Element msg)
renderFancy language count source =
    let
        parseData =
            parse language count source

        ast =
            parseData.ast

        toc_ : List (Element msg)
        toc_ =
            tableOfContents count { width = 500 } parseData.accumulator ast

        titleString =
            ASTTools.getTitle ast |> Maybe.withDefault "Untitled" |> String.replace "\n" " "

        docTitle =
            E.el [ Font.size 30 ] (E.text titleString)

        toc =
            if List.length toc_ > 1 then
                E.column [ E.paddingXY 0 24, E.spacing 8 ] toc_

            else
                E.none

        renderedText_ : List (Element msg)
        renderedText_ =
            render count { width = 500 } parseData.accumulator ast
    in
    docTitle :: toc :: renderedText_


tableOfContents : Int -> Settings -> Block.State.Accumulator -> List Block -> List (Element msg)
tableOfContents generation settings accumulator blocks =
    blocks |> ASTTools.getHeadings |> Render.Text.viewTOC generation settings accumulator


{-| -}
compile : Lang -> Int -> Settings -> List String -> List (Element msg)
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
