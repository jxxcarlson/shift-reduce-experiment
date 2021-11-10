module Render.Block exposing (render)

import Block.Accumulator exposing (Accumulator)
import Block.Block as Block exposing (Block(..), BlockStatus(..), Meta)
import Block.BlockTools as Block
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Expression.ASTTools as ASTTools
import Html.Attributes
import LaTeX.MathMacro
import Lang.Lang as Lang
import Markup.Debugger exposing (debugYellow)
import Markup.Meta exposing (ExpressionMeta)
import Render.Math
import Render.Msg exposing (MarkupMsg)
import Render.Settings exposing (Settings)
import Render.Text
import String.Extra
import Utility


htmlId str =
    Element.htmlAttribute (Html.Attributes.id str)


render : Int -> Settings -> Accumulator -> List Block -> List (Element MarkupMsg)
render generation settings accumulator blocks =
    List.map (renderBlock generation settings accumulator) blocks


highlightStyle flag =
    if not flag then
        [ Background.color (Element.rgba 0 0 0 0) ]

    else
        [ Background.color (Element.rgb 0.8 0.8 1.0), paddingXY 4 4 ]


renderBlock : Int -> Settings -> Accumulator -> Block -> Element MarkupMsg
renderBlock generation settings accumulator block =
    case block of
        Paragraph textList meta ->
            paragraph
                (highlightStyle (meta.id == settings.selectedId))
                (List.map (Render.Text.render generation settings accumulator) textList)

        VerbatimBlock name lines exprMeta meta ->
            if meta.status /= BlockComplete then
                renderLinesIncomplete settings name meta.status lines

            else
                case Dict.get name verbatimBlockDict of
                    Nothing ->
                        error ("Unimplemented verbatim block: " ++ name)

                    Just f ->
                        f generation meta.id settings accumulator lines exprMeta

        Block name blocks meta ->
            if meta.status /= BlockComplete then
                renderBlocksIncomplete settings name meta.status blocks

            else if List.member name Lang.theoremLikeNames then
                renderTheoremLikeBlock generation settings accumulator name blocks meta

            else
                case Dict.get name blockDict of
                    Nothing ->
                        -- error ("Unimplemented block: " ++ name)
                        renderBlocksIncomplete settings name BlockUnimplemented blocks

                    Just f ->
                        f generation settings accumulator blocks

        BError desc ->
            error desc


renderLinesIncomplete : Settings -> String -> BlockStatus -> List String -> Element MarkupMsg
renderLinesIncomplete settings name status lines =
    column [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ]
        [ column
            [ Font.family
                [ Font.typeface "Inconsolata"
                , Font.monospace
                ]
            , Font.color (Element.rgb 0 0 200)
            , Border.solid
            , Border.width 1
            , errorBackgroundColor settings
            , paddingXY 8 8
            , spacing 8
            ]
            (message settings.showErrorMessages name status
                :: (el [ paddingXY (verbatimPadding status) 0 ] (Element.text (errorHeaderString name))
                        :: List.map (\t -> el [] (text t)) lines
                   )
            )
        ]


verbatimPadding : BlockStatus -> Int
verbatimPadding status =
    case status of
        BlockUnfinished "indentation?" ->
            0

        _ ->
            0


errorHeaderString : String -> String
errorHeaderString name_ =
    case name_ of
        "math" ->
            "$$"

        _ ->
            "\\begin{" ++ name_ ++ "}"


renderBlocksIncomplete : Settings -> String -> BlockStatus -> List Block -> Element MarkupMsg
renderBlocksIncomplete settings name status blocks =
    column [ paddingEach { left = 20, right = 0, top = 0, bottom = 0 } ]
        [ column
            [ Font.family
                [ Font.typeface "Inconsolata"
                , Font.monospace
                ]
            , Font.color codeColor
            , Border.solid
            , Border.width 1
            , errorBackgroundColor settings
            , paddingXY 8 8
            , spacing 8
            ]
            (message settings.showErrorMessages name status
                :: el [] (Element.text (errorHeaderString name))
                :: (Element.text <| Block.stringValueOfBlockList blocks)
                :: []
            )
        ]


errorBackgroundColor settings =
    if settings.showErrorMessages then
        Background.color (Element.rgb255 230 233 250)

    else
        Background.color (Element.rgb255 250 230 233)


message : Bool -> String -> BlockStatus -> Element MarkupMsg
message show name blockStatus =
    if show then
        case blockStatus of
            BlockComplete ->
                Element.none

            MismatchedTags first second ->
                Element.el [ Font.color (Element.rgb 0 0 180) ] (Element.text <| "Mismatched tags: " ++ first ++ " ≠ " ++ second)

            BlockUnfinished "begin" ->
                Element.el [ Font.color (Element.rgb 0 0 180) ] (Element.text <| "Unfinished " ++ name ++ " block: " ++ "indentation? (9)")

            BlockUnfinished str ->
                Element.el [ Font.color (Element.rgb 0 0 180) ] (Element.text <| "Unfinished " ++ name ++ " block: " ++ str)

            BlockUnimplemented ->
                Element.el [ Font.color (Element.rgb 0 0 180) ] (Element.text <| "Unimplemented block: " ++ name)

    else
        Element.none


error str =
    paragraph [ Background.color (rgb255 250 217 215) ] [ text str ]


verbatimBlockDict : Dict String (Int -> String -> Settings -> Accumulator -> List String -> ExpressionMeta -> Element MarkupMsg)
verbatimBlockDict =
    Dict.fromList
        [ ( "code", \g id s a lines exprMeta -> codeBlock g id s a lines )
        , ( "verbatim", \g id s a lines exprMeta -> codeBlock g id s a lines )
        , ( "math", \g id s a lines exprMeta -> mathBlock g id s a lines )
        , ( "equation", \g id s a lines exprMeta -> equation g id s a lines exprMeta )
        , ( "align", \g id s a lines exprMeta -> aligned g id s a lines exprMeta )
        , ( "mathmacro", \_ _ _ _ _ _ -> Element.none )
        ]


blockDict : Dict String (Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg)
blockDict =
    Dict.fromList
        [ ( "indent", \g s a blocks -> indent g s a blocks )

        -- Used by Markdown
        , ( "quotation", \g s a blocks -> quotationBlock g s a blocks )
        , ( "itemize", \g s a blocks -> itemize g s a blocks )
        , ( "enumerate", \g s a blocks -> enumerate g s a blocks )
        , ( "thebibliography", \g s a blocks -> bibliography g s a blocks )
        , ( "title", \_ _ _ _ -> Element.none )
        , ( "heading1", \g s a blocks -> heading1 g s a blocks )
        , ( "heading2", \g s a blocks -> heading2 g s a blocks )
        , ( "heading3", \g s a blocks -> heading3 g s a blocks )
        , ( "heading4", \g s a blocks -> heading4 g s a blocks )
        ]


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


verticalPadding top bottom =
    Element.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }


heading1 g s a textList =
    Element.link [ Font.size 30, makeId textList, verticalPadding 30 30 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


heading2 g s a textList =
    Element.link [ Font.size 22, makeId textList, verticalPadding 22 22 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


heading3 : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
heading3 g s a textList =
    Element.link [ Font.size 18, makeId textList, verticalPadding 18 18 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


heading4 : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
heading4 g s a textList =
    Element.link [ Font.size 14, makeId textList, verticalPadding 14 14 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


indent : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
indent g s a textList =
    Element.column [ spacing 18, Font.size 14, makeId textList, paddingEach { left = 18, right = 0, top = 0, bottom = 0 } ]
        (List.map (renderBlock g s a) textList)


makeId : List Block -> Element.Attribute MarkupMsg
makeId blockList =
    Utility.elementAttribute "id" (Block.stringValueOfBlockList blockList |> String.trim |> makeSlug)


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


codeBlock : Int -> String -> Settings -> Accumulator -> List String -> Element MarkupMsg
codeBlock generation id settings accumulator textList =
    column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        , paddingEach { left = 0, right = 0, top = 6, bottom = 8 }
        , spacing 6
        , htmlId id
        ]
        (List.map (\t -> el [] (text t)) (List.map (String.dropLeft 0) textList))


mathBlock : Int -> String -> Settings -> Accumulator -> List String -> Element MarkupMsg
mathBlock generation id settings accumulator textList =
    Element.row [ Element.width (Element.px settings.width) ]
        [ Element.el [ Element.centerX ] (Render.Math.mathText generation id Render.Math.DisplayMathMode (String.join "\n" textList |> LaTeX.MathMacro.evalStr accumulator.macroDict))
        ]



-- Internal.MathMacro.evalStr latexState.mathMacroDictionary str


prepareMathLines : Accumulator -> List String -> String
prepareMathLines accumulator stringList =
    stringList
        |> List.filter (\line -> String.left 6 (String.trimLeft line) /= "\\label")
        |> String.join "\n"
        |> LaTeX.MathMacro.evalStr accumulator.macroDict


equation : Int -> String -> Settings -> Accumulator -> List String -> ExpressionMeta -> Element MarkupMsg
equation generation id settings accumulator textList exprMeta =
    Element.row [ Element.width (Element.px settings.width) ]
        [ Element.el [ Element.centerX ] (Render.Math.mathText generation id Render.Math.DisplayMathMode (prepareMathLines accumulator textList))
        , Element.el [ Element.alignRight, Font.size 12, equationLabelPadding ] (Element.text <| "(" ++ exprMeta.label ++ ")")
        ]


equationLabelPadding =
    Element.paddingEach { left = 0, right = 18, top = 0, bottom = 0 }


aligned : Int -> String -> Settings -> Accumulator -> List String -> ExpressionMeta -> Element MarkupMsg
aligned generation id settings accumulator textList exprMeta =
    Element.row [ Element.width (Element.px settings.width) ]
        [ Element.el [ Element.centerX ]
            (Render.Math.mathText generation id Render.Math.DisplayMathMode ("\\begin{aligned}\n" ++ (String.join "\n" textList |> LaTeX.MathMacro.evalStr accumulator.macroDict) ++ "\n\\end{aligned}"))
        , Element.el [ Element.alignRight, Font.size 12, equationLabelPadding ] (Element.text <| "(" ++ exprMeta.label ++ ")")
        ]


quotationBlock : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
quotationBlock generation settings accumulator blocks =
    column
        [ paddingEach { left = 18, right = 0, top = 0, bottom = 8 }
        ]
        (List.map (renderBlock generation settings accumulator) blocks)


renderTheoremLikeBlock : Int -> Settings -> Accumulator -> String -> List Block -> Meta -> Element MarkupMsg
renderTheoremLikeBlock generation settings accumulator name blocks meta =
    column [ Element.spacing 8 ]
        [ row [ Font.bold ] [ Element.text <| String.Extra.toTitleCase name ++ " " ++ meta.label ]
        , column
            [ Font.italic
            ]
            (List.map (renderBlock generation settings accumulator) blocks)
        ]


listSpacing =
    8


itemize : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
itemize generation settings accumulator blocks =
    let
        _ =
            debugYellow "XXX, ENTERNG itemize" (List.length blocks)
    in
    column [ spacing listSpacing ]
        (List.map (item_ generation settings accumulator) (nonEmptyBlocks blocks))


bibliography : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
bibliography generation settings accumulator blocks =
    let
        _ =
            debugYellow "XXX, ENTERNG biblography" (List.length blocks)
    in
    column [ spacing listSpacing ]
        (List.map (bibitem generation settings accumulator) (nonEmptyBlocks blocks))


bibitem : Int -> Settings -> Accumulator -> Block -> Element MarkupMsg
bibitem generation settings accumulator block =
    let
        blocks =
            case block of
                Block "bibitem" blocks_ _ ->
                    blocks_

                Paragraph [ Block.ExprM "bibitem" expressions _ ] meta ->
                    [ Paragraph expressions meta ]

                _ ->
                    []
    in
    row [ width fill, paddingEach { left = 18, right = 0, top = 0, bottom = 0 } ]
        [ el [ height fill ] none
        , column [ width fill ]
            [ row [ width fill, spacing listSpacing ]
                [ itemSymbol
                , paragraph [ width fill ] (List.map (renderBlock generation settings accumulator) blocks)
                ]
            ]
        ]


nonEmptyBlocks : List Block -> List Block
nonEmptyBlocks blocks =
    List.filter blockIsNonempty blocks


blockIsNonempty : Block -> Bool
blockIsNonempty block =
    String.length (ASTTools.stringContentOfNamedBlock block |> String.trim) > 0


item_ : Int -> Settings -> Accumulator -> Block -> Element MarkupMsg
item_ generation settings accumulator block =
    let
        blocks =
            case block of
                Block "item" blocks_ _ ->
                    blocks_

                Paragraph [ Block.ExprM "item" expressions _ ] meta ->
                    [ Paragraph expressions meta ]

                _ ->
                    []
    in
    row [ width fill, paddingEach { left = 18, right = 0, top = 0, bottom = 0 } ]
        [ el [ height fill ] none
        , column [ width fill ]
            [ row [ width fill, spacing listSpacing ]
                [ itemSymbol
                , paragraph [ width fill ] (List.map (renderBlock generation settings accumulator) blocks)
                ]
            ]
        ]


enumerate : Int -> Settings -> Accumulator -> List Block -> Element MarkupMsg
enumerate generation settings accumulator blocks =
    column [ spacing listSpacing ]
        (List.indexedMap (\k -> numberedItem_ k generation settings accumulator) (nonEmptyBlocks blocks))


numberedItem_ : Int -> Int -> Settings -> Accumulator -> Block -> Element MarkupMsg
numberedItem_ index generation settings accumulator block =
    let
        -- TODO: simplify the below, eliminating the case 'numberedItem' by takking care of these in putListItemsAsChildrenOfBlock
        blocks =
            case block of
                Block "item" blocks_ _ ->
                    blocks_

                Block "numberedItem" blocks_ _ ->
                    blocks_

                Paragraph [ Block.ExprM "item" expressions _ ] meta ->
                    [ Paragraph expressions meta ]

                Paragraph [ Block.ExprM "numberedItem" expressions _ ] meta ->
                    [ Paragraph expressions meta ]

                _ ->
                    []
    in
    row [ width fill, paddingEach { left = 18, right = 0, top = 0, bottom = 0 } ]
        [ el [ height fill ] none
        , column [ width fill ]
            [ row [ width fill, spacing listSpacing ]
                [ numberedItemSymbol index
                , paragraph [ width fill ] (List.map (renderBlock generation settings accumulator) blocks)
                ]
            ]
        ]


numberedItemSymbol k =
    el
        [ Font.size 14
        , alignTop
        ]
        (text (String.fromInt (k + 1) ++ "."))


itemSymbol =
    el [ Font.bold, alignTop, moveUp 1, Font.size 18 ] (text "•")


codeColor =
    -- E.rgb 0.2 0.5 1.0
    rgb 0.4 0 0.8
