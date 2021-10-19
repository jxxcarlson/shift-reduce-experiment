module Render.Block exposing (render)

import Block.Block as Block exposing (Block(..), BlockStatus(..))
import Block.BlockTools as Block
import Block.State
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Expression.AST
import LaTeX.MathMacro
import Markup.Debugger exposing (debugYellow)
import Render.Math
import Render.Settings exposing (Settings)
import Render.Text
import Utility



-- Internal.MathMacro.evalStr latexState.mathMacroDictionary str


render : Int -> Settings -> Block.State.Accumulator -> List Block -> List (Element msg)
render generation settings accumulator blocks =
    List.map (renderBlock generation settings accumulator) blocks


renderBlock : Int -> Settings -> Block.State.Accumulator -> Block -> Element msg
renderBlock generation settings accumulator block =
    case block of
        Paragraph textList _ ->
            paragraph
                []
                (List.map (Render.Text.render generation settings accumulator) textList)

        VerbatimBlock name lines _ meta ->
            if meta.status /= BlockComplete then
                renderLinesIncomplete name meta.status lines

            else
                case Dict.get name verbatimBlockDict of
                    Nothing ->
                        error ("Unimplemented verbatim block: " ++ name)

                    Just f ->
                        f generation settings accumulator lines

        Block name blocks meta ->
            if meta.status /= BlockComplete then
                renderBlocksIncomplete name meta.status blocks

            else
                case Dict.get name blockDict of
                    Nothing ->
                        -- error ("Unimplemented block: " ++ name)
                        renderBlocksIncomplete name BlockUnimplemented blocks

                    Just f ->
                        f generation settings accumulator blocks

        BError desc ->
            error desc


renderBlocksIncomplete : String -> BlockStatus -> List Block -> Element msg
renderBlocksIncomplete name status blocks =
    column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        , Background.color (Element.rgb255 230 233 250)
        , paddingXY 8 8
        , spacing 8
        ]
        (message name status
            :: (Element.text <| Block.stringValueOfBlockList blocks)
            :: []
        )


message : String -> BlockStatus -> Element msg
message name blockStatus =
    case blockStatus of
        BlockComplete ->
            Element.none

        MismatchedTags first second ->
            Element.el [ Font.color (Element.rgb 180 0 0) ] (Element.text <| "Mismatched tags: " ++ first ++ " ≠ " ++ second)

        BlockStarted ->
            Element.el [ Font.color (Element.rgb 180 0 0) ] (Element.text <| "Unfinished " ++ name ++ " block")

        BlockUnimplemented ->
            Element.el [ Font.color (Element.rgb 180 0 0) ] (Element.text <| "Unimplemented block: " ++ name)


renderLinesIncomplete : String -> BlockStatus -> List String -> Element msg
renderLinesIncomplete name status lines =
    column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color (Element.rgb 0 0 200)
        , Background.color (Element.rgb255 230 233 250)
        , spacing 8
        ]
        (message name status :: List.map (\t -> el [] (text t)) lines)


error str =
    paragraph [ Background.color (rgb255 250 217 215) ] [ text str ]


verbatimBlockDict : Dict String (Int -> Settings -> Block.State.Accumulator -> List String -> Element msg)
verbatimBlockDict =
    Dict.fromList
        [ ( "code", \g s a lines -> codeBlock g s a lines )
        , ( "math", \g s a lines -> mathBlock g s a lines )
        , ( "equation", \g s a lines -> equation g s a lines )
        , ( "align", \g s a lines -> aligned g s a lines )
        , ( "mathmacro", \_ _ _ _ -> Element.none )
        ]


blockDict : Dict String (Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg)
blockDict =
    Dict.fromList
        [ ( "indent", \g s a blocks -> indent g s a blocks )

        -- Used by Markdown
        , ( "quotation", \g s a blocks -> quotationBlock g s a blocks )
        , ( "itemize", \g s a blocks -> itemize g s a blocks )
        , ( "enumerate", \g s a blocks -> enumerate g s a blocks )

        --, ( "item", \g s a blocks -> item g s a blocks )
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
    Element.link [ Font.size 30, makeId textList, verticalPadding 30 15 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


heading2 g s a textList =
    Element.link [ Font.size 22, makeId textList, verticalPadding 22 11 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


heading3 : Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg
heading3 g s a textList =
    Element.link [ Font.size 18, makeId textList, verticalPadding 18 9 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


heading4 : Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg
heading4 g s a textList =
    Element.link [ Font.size 14, makeId textList, verticalPadding 14 7 ]
        { url = internalLink "TITLE", label = Element.paragraph [] (render g s a textList) }


indent : Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg
indent g s a textList =
    Element.column [ spacing 18, Font.size 14, makeId textList, paddingEach { left = 18, right = 0, top = 0, bottom = 0 } ]
        (List.map (renderBlock g s a) textList)


makeId : List Block -> Element.Attribute msg
makeId blockList =
    Utility.elementAttribute "id" (Block.stringValueOfBlockList blockList |> String.trim |> makeSlug)


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


codeBlock : Int -> Settings -> Block.State.Accumulator -> List String -> Element msg
codeBlock generation settings accumulator textList =
    column
        [ Font.family
            [ Font.typeface "Inconsolata"
            , Font.monospace
            ]
        , Font.color codeColor
        , paddingEach { left = 0, right = 0, top = 0, bottom = 8 }
        , spacing 6
        ]
        (List.map (\t -> el [] (text t)) (List.map (String.dropLeft 0) textList))


mathBlock : Int -> Settings -> Block.State.Accumulator -> List String -> Element msg
mathBlock generation settings accumulator textList =
    Render.Math.mathText generation Render.Math.DisplayMathMode (String.join "\n" textList |> LaTeX.MathMacro.evalStr accumulator.macroDict)



-- Internal.MathMacro.evalStr latexState.mathMacroDictionary str


prepareMathLines : Block.State.Accumulator -> List String -> String
prepareMathLines accumulator stringList =
    stringList
        |> List.filter (\line -> String.left 6 (String.trimLeft line) /= "\\label")
        |> String.join "\n"
        |> LaTeX.MathMacro.evalStr accumulator.macroDict


equation : Int -> Settings -> Block.State.Accumulator -> List String -> Element msg
equation generation settings accumulator textList =
    -- Render.Math.mathText generation Render.Math.DisplayMathMode (String.join "\n" textList |> MiniLaTeX.MathMacro.evalStr accumulator.macroDict)
    Render.Math.mathText generation Render.Math.DisplayMathMode (prepareMathLines accumulator textList)


aligned : Int -> Settings -> Block.State.Accumulator -> List String -> Element msg
aligned generation settings accumulator textList =
    Render.Math.mathText generation Render.Math.DisplayMathMode ("\\begin{aligned}\n" ++ (String.join "\n" textList |> LaTeX.MathMacro.evalStr accumulator.macroDict) ++ "\n\\end{aligned}")


quotationBlock : Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg
quotationBlock generation settings accumulator blocks =
    column
        [ paddingEach { left = 18, right = 0, top = 0, bottom = 8 }
        ]
        (List.map (renderBlock generation settings accumulator) (debugYellow "XX, block in quotation" blocks))


listSpacing =
    8


itemize : Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg
itemize generation settings accumulator blocks =
    let
        _ =
            debugYellow "XXX, ENTERNG itemize" (List.length blocks)
    in
    column [ spacing listSpacing ]
        (List.map (item_ generation settings accumulator) blocks)


item_ : Int -> Settings -> Block.State.Accumulator -> Block -> Element msg
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


enumerate : Int -> Settings -> Block.State.Accumulator -> List Block -> Element msg
enumerate generation settings accumulator blocks =
    let
        _ =
            debugYellow "XXX, ENTERNG ENUMERATE" (List.length blocks)
    in
    column [ spacing listSpacing ]
        (List.indexedMap (\k -> numberedItem_ k generation settings accumulator) blocks)


numberedItem_ : Int -> Int -> Settings -> Block.State.Accumulator -> Block -> Element msg
numberedItem_ index generation settings accumulator block =
    let
        -- TODO: simplify the below, eliminating the cae 'numberedItem' by takking care of these in putListItemsAsChildrenOfBlock
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
