module Markup.API exposing
    ( Settings
    , compile
    , defaultSettings
    , p
    , parse
    , prepareForExport
    , q
    , render
    , renderFancy
    , rl
    , tableOfContents
    )

import Block.Accumulator as Accumulator exposing (Accumulator)
import Block.Block exposing (Block, ExprM(..), SBlock)
import Block.BlockTools
import Block.Function
import Block.Parser
import Element as E exposing (Element)
import Element.Font as Font
import Expression.ASTTools as ASTTools
import Expression.Parser
import LaTeX.Export.Markdown
import Lang.Lang exposing (Lang(..))
import Markup.Simplify as Simplify
import Markup.Vector as Vector
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


q : Lang -> String -> List Block
q lang str =
    parse lang 0 (String.lines str) |> .ast


rl : String -> List (Element MarkupMsg)
rl str =
    renderFancy defaultSettings L1 0 (String.lines str)



-- NOTE THE AST TRANSFORMATION BELOW


parse : Lang -> Int -> List String -> { ast : List Block, accumulator : Accumulator }
parse lang generation lines =
    let
        state =
            Block.Parser.run lang generation lines

        data =
            List.foldl (folder lang) { accumulator = Accumulator.init 4, blocks = [] } state.committed

        ast =
            case lang of
                Markdown ->
                    data.blocks
                        |> List.reverse
                        |> LaTeX.Export.Markdown.putListItemsAsChildrenOfBlock

                _ ->
                    data.blocks |> List.reverse
    in
    { ast = ast
    , accumulator = data.accumulator
    }


folder : Lang -> SBlock -> { accumulator : Accumulator, blocks : List Block } -> { accumulator : Accumulator, blocks : List Block }
folder lang sblock acc =
    let
        block =
            sblockToBlock lang acc.accumulator sblock

        data =
            labelBlock acc.accumulator block
    in
    { accumulator = Accumulator.updateAccumulatorWithBlock block data.accumulator, blocks = data.block :: acc.blocks }


sblockToBlock : Lang -> Accumulator -> SBlock -> Block
sblockToBlock lang accumulator sblock =
    (Block.BlockTools.map (Expression.Parser.parseExpr lang) >> Block.Function.fixMarkdownBlock) sblock



--|> label accumulator


labelBlock : Accumulator -> Block -> { block : Block, accumulator : Accumulator }
labelBlock accumulator block =
    case block of
        Block.Block.Paragraph exprList meta ->
            List.foldl xfolder { expressions = [], accumulator = accumulator } exprList
                |> (\data -> { block = Block.Block.Paragraph (data.expressions |> List.reverse) meta, accumulator = data.accumulator })

        _ ->
            { block = block, accumulator = accumulator }


xfolder : ExprM -> { expressions : List ExprM, accumulator : Accumulator } -> { expressions : List ExprM, accumulator : Accumulator }
xfolder expr data =
    labelExpression data.accumulator expr
        |> (\result -> { expressions = result.expr :: data.expressions, accumulator = result.accumulator })


labelExpression : Accumulator -> ExprM -> { expr : ExprM, accumulator : Accumulator }
labelExpression accumulator expr =
    case expr of
        ExprM name exprList exprMeta ->
            let
                data =
                    labelForName name accumulator
            in
            { expr = ExprM name (List.map (setLabel data.label) exprList) { exprMeta | label = data.label }, accumulator = data.accumulator }

        _ ->
            { expr = expr, accumulator = accumulator }


setLabel : String -> ExprM -> ExprM
setLabel label expr =
    case expr of
        TextM str exprMeta ->
            TextM str { exprMeta | label = label }

        VerbatimM name str exprMeta ->
            VerbatimM name str { exprMeta | label = label }

        ArgM args exprMeta ->
            ArgM args { exprMeta | label = label }

        ExprM name args exprMeta ->
            ExprM name args { exprMeta | label = label }

        ErrorM str ->
            ErrorM str


labelForName : String -> Accumulator -> { label : String, accumulator : Accumulator }
labelForName str accumulator =
    case str of
        "heading1" ->
            let
                sectionIndex =
                    Vector.increment 0 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        "heading2" ->
            let
                sectionIndex =
                    Vector.increment 1 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        "heading3" ->
            let
                sectionIndex =
                    Vector.increment 2 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        "heading4" ->
            let
                sectionIndex =
                    Vector.increment 3 accumulator.sectionIndex
            in
            { label = Vector.toString sectionIndex, accumulator = { accumulator | sectionIndex = sectionIndex } }

        _ ->
            { label = str, accumulator = accumulator }


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


tableOfContents : Int -> Settings -> Accumulator -> List Block -> List (Element MarkupMsg)
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
