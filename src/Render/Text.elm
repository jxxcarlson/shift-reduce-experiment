module Render.Text exposing (heading1, render, viewTOC)

import Block.Accumulator exposing (Accumulator)
import Block.Block exposing (ExprM(..))
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, newTabLink, px, spacing)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Expression.AST exposing (Expr(..))
import Expression.ASTTools as ASTTools
import LaTeX.MathMacro
import Render.Math
import Render.Msg exposing (MarkupMsg(..))
import Render.Settings exposing (Settings)
import Utility


render : Int -> Settings -> Accumulator -> ExprM -> Element MarkupMsg
render generation settings accumulator text =
    case text of
        TextM string meta ->
            Element.el [ Events.onClick (SendMeta meta) ] (Element.text string)

        ExprM name exprList _ ->
            Element.el [] (renderMarked name generation settings accumulator exprList)

        VerbatimM name str _ ->
            renderVerbatim name generation settings accumulator str

        ArgM _ _ ->
            Element.none

        ErrorM str ->
            Element.el [ Font.color redColor ] (Element.text str)


errorText index str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text <| "(" ++ String.fromInt index ++ ") not implemented: " ++ str)


renderVerbatim name generation settings accumulator str =
    case Dict.get name verbatimDict of
        Nothing ->
            errorText 1 name

        Just f ->
            f generation settings accumulator str


renderMarked name generation settings accumulator exprList =
    case Dict.get name markupDict of
        Nothing ->
            Element.el [ Font.color errorColor ] (Element.text name)

        Just f ->
            f generation settings accumulator exprList


markupDict : Dict String (Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg)
markupDict =
    Dict.fromList
        [ ( "special", \g s a exprList -> special g s a exprList )
        , ( "item", \g s a exprList -> item g s a exprList )
        , ( "numberedItem", \g s a exprList -> numberedItem g s a exprList )
        , ( "strong", \g s a exprList -> strong g s a exprList )
        , ( "bold", \g s a exprList -> strong g s a exprList )
        , ( "italic", \g s a exprList -> italic g s a exprList )
        , ( "boldItalic", \g s a exprList -> boldItalic g s a exprList )
        , ( "red", \g s a exprList -> red g s a exprList )
        , ( "blue", \g s a exprList -> blue g s a exprList )
        , ( "violet", \g s a exprList -> violet g s a exprList )
        , ( "highlight", \g s a exprList -> highlight g s a exprList )
        , ( "strike", \g s a exprList -> strike g s a exprList )
        , ( "underline", \g s a exprList -> underline g s a exprList )
        , ( "gray", \g s a exprList -> gray g s a exprList )
        , ( "errorHighlight", \g s a exprList -> errorHighlight g s a exprList )
        , ( "title", \_ _ _ _ -> Element.none )
        , ( "heading1", \g s a exprList -> heading1 g s a exprList )
        , ( "heading2", \g s a exprList -> heading2 g s a exprList )
        , ( "heading3", \g s a exprList -> heading3 g s a exprList )
        , ( "heading4", \g s a exprList -> heading4 g s a exprList )
        , ( "heading5", \g s a exprList -> italic g s a exprList )
        , ( "skip", \g s a exprList -> skip g s a exprList )
        , ( "link", \g s a exprList -> link g s a exprList )
        , ( "xlink", \g s a exprList -> xlink g s a exprList )
        , ( "href", \g s a exprList -> href g s a exprList )
        , ( "image", \g s a exprList -> image g s a exprList )
        , ( "texmacro", \g s a exprList -> texmacro g s a exprList )
        , ( "texarg", \g s a exprList -> texarg g s a exprList )
        , ( "abstract", \g s a exprList -> abstract g s a exprList )
        , ( "tags", \g s a exprList -> Element.none )
        , ( "author", \g s a exprList -> Element.none )
        , ( "date", \g s a exprList -> Element.none )
        , ( "large", \g s a exprList -> large g s a exprList )

        -- MiniLaTeX stuff
        , ( "term", \g s a exprList -> term g s a exprList )
        , ( "emph", \g s a exprList -> emph g s a exprList )
        , ( "eqref", \g s a exprList -> eqref g s a exprList )
        , ( "setcounter", \_ _ _ _ -> Element.none )
        ]


verbatimDict : Dict String (Int -> Settings -> Accumulator -> String -> Element MarkupMsg)
verbatimDict =
    Dict.fromList
        [ ( "$", \g s a str -> math g s a str )
        , ( "`", \g s a str -> code g s a str )
        , ( "code", \g s a str -> code g s a str )
        , ( "math", \g s a str -> math g s a str )
        ]


special : Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
special g s a exprList =
    case exprList of
        (TextM functionName _) :: (TextM argString _) :: [] ->
            case Dict.get functionName specialFunctionsDict of
                Nothing ->
                    Element.paragraph []
                        [ Element.el [ Font.color redColor ] (Element.text <| functionName ++ ": ")
                        , Element.el [ Font.color blueColor ] (Element.text argString)
                        ]

                Just f ->
                    f s argString

        _ ->
            Element.paragraph []
                [ Element.el [ Font.color redColor ] (Element.text "Bad syntax for special function")
                ]


specialFunctionsDict : Dict String (Settings -> String -> Element MarkupMsg)
specialFunctionsDict =
    Dict.fromList
        [ ( "title", \s str -> title s str )
        , ( "red", \s str -> Element.el [ Font.color redColor ] (Element.text str) )
        , ( "blue", \s str -> Element.el [ Font.color blueColor ] (Element.text str) )
        ]


redColor =
    Element.rgb 0.6 0 0.8


blueColor =
    Element.rgb 0 0 0.8


texmacro : Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
texmacro g s a exprList =
    macro1 (\str -> Element.el [] (Element.text ("\\" ++ str))) g s a exprList


texarg g s a exprList =
    macro1 (\str -> Element.el [] (Element.text ("{" ++ str ++ "}"))) g s a exprList


macro1 : (String -> Element MarkupMsg) -> Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
macro1 f g s a exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: _ ->
            f arg1

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


macro2 : (String -> String -> Element MarkupMsg) -> Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
macro2 element g s a exprList =
    case ASTTools.exprListToStringList exprList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: arg2 :: _ ->
            element arg1 arg2

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


large g s a exprList =
    simpleElement [ Font.size 18 ] g s a exprList


author g s a exprList =
    simpleElement [ Font.size 18 ] g s a exprList


abstract g s a exprList =
    Element.paragraph [] [ Element.el [ Font.size 18 ] (Element.text "Abstract."), simpleElement [] g s a exprList ]


xlink g s a exprList =
    case exprList of
        (TextM label _) :: (TextM docId _) :: _ ->
            xlink_ docId label

        _ ->
            el [ Font.color errorColor ] (Element.text "bad data for link")


xlink_ : String -> String -> Element MarkupMsg
xlink_ docId label =
    Input.button []
        { onPress = Just (GetPublicDocument docId)
        , label = Element.el [ Element.centerX, Element.centerY, Font.size 14, Font.color (Element.rgb 0 0 0.8) ] (Element.text label)
        }


link g s a exprList =
    case exprList of
        (TextM label _) :: (TextM url _) :: _ ->
            link_ url label

        _ ->
            el [ Font.color errorColor ] (Element.text "bad data for link")


link_ : String -> String -> Element MarkupMsg
link_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor ] (Element.text label)
        }


href g s a exprList =
    macro2 href_ g s a exprList


href_ : String -> String -> Element MarkupMsg
href_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (Element.text <| label)
        }



--         , ( "href", \g s a exprList -> href g s a exprList )


image generation settings accumuator body =
    let
        arguments =
            ASTTools.exprListToStringList body

        url =
            List.head arguments |> Maybe.withDefault "no-image"

        dict =
            Utility.keyValueDict (List.drop 1 arguments)

        description =
            Dict.get "caption" dict |> Maybe.withDefault ""

        caption =
            case Dict.get "caption" dict of
                Nothing ->
                    Element.none

                Just c ->
                    Element.row [ placement, Element.width Element.fill ] [ el [ Element.width Element.fill ] (Element.text c) ]

        width =
            case Dict.get "width" dict of
                Nothing ->
                    px displayWidth

                Just w_ ->
                    case String.toInt w_ of
                        Nothing ->
                            px displayWidth

                        Just w ->
                            px w

        placement =
            case Dict.get "placement" dict of
                Nothing ->
                    centerX

                Just "left" ->
                    alignLeft

                Just "right" ->
                    alignRight

                Just "center" ->
                    centerX

                _ ->
                    centerX

        displayWidth =
            settings.width
    in
    column [ spacing 8, Element.width (px settings.width), placement, Element.paddingXY 0 18 ]
        [ Element.image [ Element.width width, placement ]
            { src = url, description = description }
        , caption
        ]


errorColor =
    Element.rgb 0.8 0 0


linkColor =
    Element.rgb 0 0 0.8


simpleElement : List (Element.Attribute MarkupMsg) -> Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
simpleElement formatList g s a exprList =
    Element.paragraph formatList (List.map (render g s a) exprList)


verbatimElement formatList g s a str =
    Element.el formatList (Element.text str)


code g s a str =
    verbatimElement codeStyle g s a str


math g s a str =
    mathElement g s a str


codeStyle =
    [ Font.family
        [ Font.typeface "Inconsolata"
        , Font.monospace
        ]
    , Font.color codeColor
    , Element.paddingEach { left = 2, right = 2, top = 0, bottom = 0 }
    ]


mathElement : Int -> Settings -> Accumulator -> String -> Element MarkupMsg
mathElement generation settings accumulator str =
    Render.Math.mathText generation Render.Math.InlineMathMode (LaTeX.MathMacro.evalStr accumulator.macroDict str)


item : Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
item generation settings accumulator str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " ") ]


numberedItem : Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
numberedItem generation settings accumulator str =
    Element.paragraph [ Element.width Element.fill ] [ Element.text (ASTTools.exprListToStringList str |> String.join " ") ]


itemSymbol =
    el [ Font.bold, Element.alignTop, Element.moveUp 1, Font.size 18 ] (Element.text "•")


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


tocColor =
    Element.rgb 0.1 0 0.8


viewTOC : Int -> Settings -> Accumulator -> List ExprM -> List (Element MarkupMsg)
viewTOC generation settings accumulator items =
    Element.el [ Font.size 18 ] (Element.text "Contents") :: List.map (viewTOCItem generation settings accumulator) items


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


tocLink : List ExprM -> Element MarkupMsg
tocLink exprList =
    let
        t =
            ASTTools.stringValueOfList exprList
    in
    Element.link [] { url = internalLink t, label = Element.text t }


viewTOCItem : Int -> Settings -> Accumulator -> ExprM -> Element MarkupMsg
viewTOCItem generation settings accumulator block =
    case block of
        ExprM "heading1" exprList _ ->
            el (tocStyle 1 exprList) (tocLink exprList)

        ExprM "heading2" exprList _ ->
            el (tocStyle 2 exprList) (tocLink exprList)

        ExprM "heading3" exprList _ ->
            el (tocStyle 3 exprList) (tocLink exprList)

        ExprM "heading4" exprList _ ->
            el (tocStyle 4 exprList) (tocLink exprList)

        ExprM "heading5" exprList _ ->
            el (tocStyle 5 exprList) (tocLink exprList)

        _ ->
            Element.none


tocStyle k exprList =
    [ Font.size 14, Font.color tocColor, leftPadding (k * tocPadding) ]


leftPadding k =
    Element.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


tocPadding =
    8


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


makeId : List ExprM -> Element.Attribute MarkupMsg
makeId exprList =
    Utility.elementAttribute "id" (ASTTools.stringValueOfList exprList |> String.trim |> makeSlug)


title : Settings -> String -> Element MarkupMsg
title s titleText =
    -- el [ Font.size s.titleSize, Utility.elementAttribute "id" titleText ] (Element.text titleText)
    Element.none


verticalPadding top bottom =
    Element.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }


headingFontSize settings level =
    let
        factor =
            (sqrt (level + 1) - 0.2) |> min 2.2

        size =
            toFloat settings.titleSize / factor |> round
    in
    Font.size size


heading1 : Int -> Settings -> Accumulator -> List ExprM -> Element MarkupMsg
heading1 g s a exprList =
    Element.column [ headingFontSize s 1, verticalPadding 14 0, makeId exprList ]
        [ Element.link []
            { url = internalLink "TITLE", label = Element.paragraph [] (elementLabel exprList :: List.map (render g s a) exprList) }
        ]


elementLabel : List ExprM -> Element msg
elementLabel expressions =
    Element.el [] (Element.text (labelOf expressions ++ " "))


labelOf : List ExprM -> String
labelOf expressions =
    case List.head expressions of
        Nothing ->
            ""

        Just (TextM _ meta) ->
            meta.label

        _ ->
            ""


heading2 g s a exprList =
    Element.column [ headingFontSize s 2, verticalPadding 14 0, makeId exprList ]
        [ Element.link []
            { url = internalLink "TITLE", label = Element.paragraph [] (elementLabel exprList :: List.map (render g s a) exprList) }
        ]


heading3 g s a exprList =
    Element.column [ headingFontSize s 3, verticalPadding 14 0, makeId exprList ]
        [ Element.link []
            { url = internalLink "TITLE", label = Element.paragraph [] (elementLabel exprList :: List.map (render g s a) exprList) }
        ]


heading4 g s a exprList =
    Element.column [ headingFontSize s 4, verticalPadding 14 0, makeId exprList ]
        [ Element.link []
            { url = internalLink "TITLE", label = Element.paragraph [] (elementLabel exprList :: List.map (render g s a) exprList) }
        ]


heading5 g s a exprList =
    Element.column [ headingFontSize s 5, verticalPadding 14 0, makeId exprList ]
        [ Element.link []
            { url = internalLink "TITLE", label = Element.paragraph [] (elementLabel exprList :: List.map (render g s a) exprList) }
        ]


skip g s a exprList =
    let
        numVal : String -> Int
        numVal str =
            String.toInt str |> Maybe.withDefault 0

        f : String -> Element MarkupMsg
        f str =
            column [ Element.spacingXY 0 (numVal str) ] [ Element.text "" ]
    in
    macro1 f g s a exprList


strong g s a exprList =
    simpleElement [ Font.bold ] g s a exprList


italic g s a exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a exprList


boldItalic g s a exprList =
    simpleElement [ Font.italic, Font.bold, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a exprList


term g s a exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a exprList


eqref g s a exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a exprList


emph g s a exprList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a exprList


red g s a exprList =
    simpleElement [ Font.color (Element.rgb255 200 0 0) ] g s a exprList


blue g s a exprList =
    simpleElement [ Font.color (Element.rgb255 0 0 200) ] g s a exprList


violet g s a exprList =
    simpleElement [ Font.color (Element.rgb255 150 100 255) ] g s a exprList


highlight g s a exprList =
    simpleElement [ Background.color (Element.rgb255 255 255 0) ] g s a exprList


strike g s a exprList =
    simpleElement [ Font.strike ] g s a exprList


underline g s a exprList =
    simpleElement [ Font.underline ] g s a exprList


gray g s a exprList =
    simpleElement [ Font.color (Element.rgb 0.5 0.5 0.5) ] g s a exprList


errorHighlight g s a exprList =
    simpleElement [ Background.color (Element.rgb255 255 200 200), Element.paddingXY 2 2 ] g s a exprList
