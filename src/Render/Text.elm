module Render.Text exposing (render, viewTOC)

import Block.Block exposing (ExprM(..))
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, column, el, newTabLink, px, spacing)
import Element.Background as Background
import Element.Font as Font
import Expression.ASTTools as ASTTools
import Render.AST2
import Render.Math
import Render.MathMacro
import Render.Settings exposing (Settings)
import Utility


type alias Accumulator =
    { macroDict : Render.MathMacro.MathMacroDict }


render : Int -> Settings -> Accumulator -> ExprM -> Element msg
render generation settings accumulator text =
    case text of
        TextM string _ ->
            Element.el [] (Element.text string)

        ExprM name textList _ ->
            Element.el [] (renderMarked name generation settings accumulator textList)

        VerbatimM name str _ ->
            renderVerbatim name generation settings accumulator str

        ArgM _ _ ->
            Element.none


errorText index str =
    Element.el [ Font.color (Element.rgb255 200 40 40) ] (Element.text <| "(" ++ String.fromInt index ++ ") not implemented: " ++ str)


renderVerbatim name generation settings accumulator str =
    case Dict.get name verbatimDict of
        Nothing ->
            errorText 1 name

        Just f ->
            f generation settings accumulator str


renderMarked name generation settings accumulator textList =
    case Dict.get name markupDict of
        Nothing ->
            Element.el [ Font.color errorColor ] (Element.text name)

        Just f ->
            f generation settings accumulator textList


markupDict : Dict String (Int -> Settings -> Accumulator -> List ExprM -> Element msg)
markupDict =
    Dict.fromList
        [ ( "strong", \g s a textList -> strong g s a textList )
        , ( "bold", \g s a textList -> strong g s a textList )
        , ( "italic", \g s a textList -> italic g s a textList )
        , ( "boldItalic", \g s a textList -> boldItalic g s a textList )
        , ( "red", \g s a textList -> red g s a textList )
        , ( "blue", \g s a textList -> blue g s a textList )
        , ( "violet", \g s a textList -> violet g s a textList )
        , ( "errorHighlight", \g s a textList -> errorHighlight g s a textList )
        , ( "title", \_ _ _ _ -> Element.none )
        , ( "heading1", \g s a textList -> heading1 g s a textList )
        , ( "heading2", \g s a textList -> heading2 g s a textList )
        , ( "heading3", \g s a textList -> heading3 g s a textList )
        , ( "heading4", \g s a textList -> heading4 g s a textList )
        , ( "heading5", \g s a textList -> italic g s a textList )
        , ( "skip", \g s a textList -> skip g s a textList )
        , ( "link", \g s a textList -> link g s a textList )
        , ( "href", \g s a textList -> href g s a textList )
        , ( "image", \g s a textList -> image g s a textList )
        , ( "texmacro", \g s a textList -> texmacro g s a textList )
        , ( "texarg", \g s a textList -> texarg g s a textList )

        -- MiniLaTeX stuff
        , ( "term", \g s a textList -> term g s a textList )
        , ( "emph", \g s a textList -> emph g s a textList )
        , ( "eqref", \g s a textList -> eqref g s a textList )
        , ( "setcounter", \_ _ _ _ -> Element.none )
        ]


verbatimDict : Dict String (Int -> Settings -> Accumulator -> String -> Element msg)
verbatimDict =
    Dict.fromList
        [ ( "$", \g s a str -> math g s a str )
        , ( "`", \g s a str -> code g s a str )
        , ( "code", \g s a str -> code g s a str )
        , ( "math", \g s a str -> math g s a str )
        ]


texmacro g s a textList =
    macro1 (\str -> Element.el [] (Element.text ("\\" ++ str))) g s a textList


texarg g s a textList =
    macro1 (\str -> Element.el [] (Element.text ("{" ++ str ++ "}"))) g s a textList


macro1 : (String -> Element msg) -> Int -> Settings -> Accumulator -> List ExprM -> Element msg
macro1 f g s a textList =
    case ASTTools.exprListToStringList textList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: _ ->
            f arg1

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


macro2 : (String -> String -> Element msg) -> Int -> Settings -> Accumulator -> List ExprM -> Element msg
macro2 element g s a textList =
    case ASTTools.exprListToStringList textList of
        -- TODO: temporary fix: parse is producing the args in reverse order
        arg1 :: arg2 :: _ ->
            element arg1 arg2

        _ ->
            el [ Font.color errorColor ] (Element.text "Invalid arguments")


link g s a exprList =
    case exprList of
        (TextM label _) :: (TextM url _) :: _ ->
            link_ url label

        _ ->
            el [ Font.color errorColor ] (Element.text "bad data for link")


link_ : String -> String -> Element msg
link_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (Element.text label)
        }


href g s a textList =
    macro2 href_ g s a textList


href_ : String -> String -> Element msg
href_ url label =
    newTabLink []
        { url = url
        , label = el [ Font.color linkColor, Font.italic ] (Element.text <| label)
        }



--         , ( "href", \g s a textList -> href g s a textList )


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


simpleElement formatList g s a textList =
    Element.paragraph formatList (List.map (render g s a) textList)


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


mathElement : Int -> Settings -> Accumulator -> String -> Element msg
mathElement generation settings accumulator str =
    Render.Math.mathText generation Render.Math.InlineMathMode (Render.MathMacro.evalStr accumulator.macroDict str)


codeColor =
    -- E.rgb 0.2 0.5 1.0
    Element.rgb 0.4 0 0.8


tocColor =
    Element.rgb 0.1 0 0.8


viewTOC : Int -> Settings -> Accumulator -> List ExprM -> List (Element msg)
viewTOC generation settings accumulator items =
    Element.el [ Font.size 18 ] (Element.text "Contents") :: List.map (viewTOCItem generation settings accumulator) items


internalLink : String -> String
internalLink str =
    "#" ++ str |> makeSlug


tocLink : List ExprM -> Element msg
tocLink textList =
    let
        t =
            Render.AST2.stringValueOfList textList
    in
    Element.link [] { url = internalLink t, label = Element.text t }


viewTOCItem : Int -> Settings -> Accumulator -> ExprM -> Element msg
viewTOCItem generation settings accumulator block =
    case block of
        ExprM "heading2" textList _ ->
            el (tocStyle 2 textList) (tocLink textList)

        ExprM "heading3" textList _ ->
            el (tocStyle 3 textList) (tocLink textList)

        ExprM "heading4" textList _ ->
            el (tocStyle 4 textList) (tocLink textList)

        ExprM "heading5" textList _ ->
            el (tocStyle 5 textList) (tocLink textList)

        _ ->
            Element.none


tocStyle k textList =
    [ Font.size 14, Font.color tocColor, leftPadding (k * tocPadding), makeId textList ]


leftPadding k =
    Element.paddingEach { left = k, right = 0, top = 0, bottom = 0 }


tocPadding =
    8


makeSlug : String -> String
makeSlug str =
    str |> String.toLower |> String.replace " " "-"


makeId : List ExprM -> Element.Attribute msg
makeId textList =
    Utility.elementAttribute "id" (Render.AST2.stringValueOfList textList |> String.trim |> makeSlug)


heading1 g s a textList =
    simpleElement [ Font.size s.titleSize, makeId textList ] g s a textList


verticalPadding top bottom =
    Element.paddingEach { top = top, bottom = bottom, left = 0, right = 0 }


heading2 g s a textList =
    Element.column [ Font.size 22, verticalPadding 22 11 ]
        [ Element.link [ makeId textList ]
            { url = internalLink "TITLE", label = Element.paragraph [] (List.map (render g s a) textList) }
        ]


heading3 g s a textList =
    Element.column [ Font.size 18, verticalPadding 18 9 ]
        [ Element.link [ makeId textList ]
            { url = internalLink "TITLE", label = Element.paragraph [] (List.map (render g s a) textList) }
        ]


heading4 g s a textList =
    Element.column [ Font.size 14, verticalPadding 14 7 ]
        [ Element.link [ makeId textList ]
            { url = internalLink "TITLE", label = Element.paragraph [] (List.map (render g s a) textList) }
        ]


skip g s a textList =
    let
        numVal : String -> Int
        numVal str =
            String.toInt str |> Maybe.withDefault 0

        f : String -> Element msg
        f str =
            column [ Element.spacingXY 0 (numVal str) ] [ Element.text "-" ]
    in
    macro1 f g s a textList


strong g s a textList =
    simpleElement [ Font.bold ] g s a textList


italic g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


boldItalic g s a textList =
    simpleElement [ Font.italic, Font.bold, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


term g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


eqref g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


emph g s a textList =
    simpleElement [ Font.italic, Element.paddingEach { left = 0, right = 2, top = 0, bottom = 0 } ] g s a textList


red g s a textList =
    simpleElement [ Font.color (Element.rgb255 200 0 0) ] g s a textList


blue g s a textList =
    simpleElement [ Font.color (Element.rgb255 0 0 200) ] g s a textList


violet g s a textList =
    simpleElement [ Font.color (Element.rgb255 150 100 255) ] g s a textList


errorHighlight g s a textList =
    simpleElement [ Background.color (Element.rgb255 255 200 200), Element.paddingXY 2 2 ] g s a textList
