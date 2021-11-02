module Main exposing (main)

import Block.Accumulator as Accumulator exposing (Accumulator)
import Block.Block exposing (Block)
import Browser
import Data.L1Test
import Data.MarkdownTest
import Data.MiniLaTeXTest
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Expression.ASTTools as ASTTools
import File.Download as Download
import Html exposing (Html)
import Html.Attributes as HtmlAttr exposing (attribute)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode
import Json.Encode
import LaTeX.Export.API
import LaTeX.Export.Block
import Lang.Lang exposing (Lang(..))
import Markup.API as API exposing (defaultSettings)
import Markup.Meta
import Process
import Render.Msg
import Render.Settings
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { sourceText : String
    , language : Lang
    , count : Int
    , windowHeight : Int
    , windowWidth : Int
    , viewMode : ViewMode
    , message : String
    , lineNumber : Int
    , searchText : String
    , searchCount : Int
    , parseData : { ast : List Block, accumulator : Accumulator }
    }


type ViewMode
    = StandardView
    | LaTeXSource


viewModeToString : ViewMode -> String
viewModeToString mode =
    case mode of
        StandardView ->
            "Standard view"

        LaTeXSource ->
            "LaTeX"


type Msg
    = NoOp
    | InputText String
    | InputSearch String
    | Search
    | ClearText
    | LoadDocumentText Lang String
    | IncrementCounter
    | SetViewMode ViewMode
    | Render Render.Msg.MarkupMsg


identifierToLanguage : String -> Lang
identifierToLanguage str =
    case str of
        "l1doc" ->
            L1

        "minilatexDoc" ->
            MiniLaTeX

        "markdownDoc" ->
            Markdown

        _ ->
            Markdown


type alias Flags =
    { width : Int, height : Int }


initialText =
    Data.MiniLaTeXTest.text


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { sourceText = initialText
      , language = MiniLaTeX
      , count = 0
      , windowHeight = flags.height
      , windowWidth = flags.width
      , viewMode = StandardView
      , message = ""
      , lineNumber = 0
      , searchText = ""
      , searchCount = 0
      , parseData = { ast = [], accumulator = Accumulator.init 4 }
      }
    , Process.sleep 100 |> Task.perform (always IncrementCounter)
    )


renderArgs =
    { width = 450
    , selectedId = "foobar"
    , generation = 0
    }


subscriptions model =
    Sub.none



--
--ChangePlanText s ->
--( { model | currPlanText = s }, Cmd.none )
--
--SubmitPlan ->
--( { model | currPage = DisplayPage }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetViewMode viewMode ->
            ( { model | viewMode = viewMode }, Cmd.none )

        InputText str ->
            ( { model
                | sourceText = str
                , parseData = API.parse model.language model.count (String.lines str)
                , count = model.count + 1
              }
            , Cmd.none
            )

        InputSearch str ->
            ( { model | searchText = str }, Cmd.none )

        Search ->
            ( { model | searchCount = model.searchCount + 1, message = ASTTools.findIdsMatchingText model.searchText model.parseData.ast |> Debug.toString }, Cmd.none )

        ClearText ->
            ( { model
                | sourceText = ""
                , count = model.count + 1
              }
            , Cmd.none
            )

        LoadDocumentText language text ->
            ( { model | sourceText = text, language = language, count = model.count + 1 }, Cmd.none )

        IncrementCounter ->
            ( model, Cmd.none )

        Render msg_ ->
            case msg_ of
                Render.Msg.SendMeta m ->
                    ( { model | lineNumber = m.loc.begin.row, message = "line " ++ String.fromInt (m.loc.begin.row + 1) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


download : String -> String -> String -> Cmd msg
download filename mimetype content =
    Download.string filename mimetype content



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ focusStyle noFocus ] } [ bgGray 0.2, clipX, clipY ] (mainColumn model)


noFocus : Element.FocusStyle
noFocus =
    { borderColor = Nothing
    , backgroundColor = Nothing
    , shadow = Nothing
    }


mainColumn : Model -> Element Msg
mainColumn model =
    column (mainColumnStyle model)
        [ column [ centerY, paddingEach { top = 46, bottom = 0, left = 0, right = 0 }, spacing 8, width (px appWidth_), height (px (appHeight_ model)) ]
            [ -- title "L3 Demo App"
              column [ height (panelHeight model), spacing 12 ]
                [ row [ spacing 12 ] [ editor model, rhs model ]
                ]
            , row [ Element.paddingXY 8 0, Element.height (px 30), Element.width fill, Font.size 14, Background.color (Element.rgb 0.3 0.3 0.3), Font.color (Element.rgb 1 1 1) ] [ Element.text model.message ]
            ]
        ]



-- PARAMETERS


widePanelWidth_ =
    2 * panelWidth_


panelWidth_ =
    560


appHeight_ model =
    model.windowHeight - 140


panelHeight model =
    px (appHeight_ model - 160)


innerPanelHeight model =
    appHeight_ model - 180


appWidth_ =
    2 * panelWidth_ + 15


editor model =
    column [ height (px (innerPanelHeight model)), moveUp 28 ]
        [ row [ spacing 12 ]
            [ l1DocButton model.language
            , miniLaTeXDocButton model.language
            , markdownDocButton model.language
            , searchField model
            ]
        , editor_ model
        ]


searchField : Model -> Element Msg
searchField model =
    inputFieldTemplate [ onEnter Search |> Element.htmlAttribute ] Element.fill "Search ..." InputSearch model.searchText


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail "not ENTER"
    in
    on "keydown" (keyCode |> Json.Decode.andThen isEnter)


inputFieldTemplate attr width_ default msg text =
    Input.text ([ Element.moveUp 5, Font.size 16, Element.height (px 33), Element.width width_ ] ++ attr)
        { onChange = msg
        , text = text
        , label = Input.labelHidden default
        , placeholder = Just <| Input.placeholder [ Element.moveUp 5 ] (Element.text default)
        }


editor_ : Model -> Element Msg
editor_ model =
    let
        onChange : Html.Attribute Msg
        onChange =
            Json.Decode.string
                |> Json.Decode.at [ "target", "editorText" ]
                |> Json.Decode.map InputText
                |> Html.Events.on "change"
    in
    el [ htmlAttribute onChange ] <|
        html <|
            Html.node "ace-editor"
                [ HtmlAttr.attribute "theme" "twilight"
                , HtmlAttr.attribute "wrapmode" "true"
                , HtmlAttr.attribute "tabsize" "2"
                , HtmlAttr.attribute "linenumber" (String.fromInt (model.lineNumber + 1))
                , HtmlAttr.attribute "softtabs" "true"
                , HtmlAttr.attribute "navigateWithinSoftTabs" "true"
                , HtmlAttr.attribute "fontsize" "12"
                , HtmlAttr.style "height" (String.fromInt (innerPanelHeight model) ++ "px")
                , HtmlAttr.style "width" (String.fromInt panelWidth_ ++ "px")
                , HtmlAttr.attribute "text" model.sourceText
                , HtmlAttr.attribute "searchkey" model.searchText
                , HtmlAttr.attribute "searchcount" (String.fromInt model.searchCount |> Debug.log "SEARCHCOUNT")
                ]
                []


green =
    Element.rgb 0 1 1


darkGreen =
    Element.rgb 0 0.4 0.4


white =
    Element.rgb 1 1 1


keyIt : Int -> List b -> List ( String, b )
keyIt k list =
    List.indexedMap (\i e -> ( String.fromInt (i + k), e )) list


title : String -> Element msg
title str =
    row [ centerX, fontGray 0.9 ] [ text str ]


rhs : Model -> Element Msg
rhs model =
    column [ spacing 8 ]
        [ row
            [ fontGray 0.9
            , spacing 12
            , moveUp 9
            , Font.size 14
            ]
            [ dummyButton
            , text ("generation: " ++ String.fromInt model.count)
            , wordCountElement model.sourceText
            , setViewMode model StandardView
            , setViewMode model LaTeXSource
            ]
        , case model.viewMode of
            StandardView ->
                renderedText model

            LaTeXSource ->
                latexSourceView model
        ]


wordCount : String -> Int
wordCount str =
    str |> String.words |> List.length


wordCountElement : String -> Element Msg
wordCountElement str =
    row [ spacing 8 ] [ el [] (text <| "words:"), el [] (text <| String.fromInt <| wordCount <| str) ]


renderedText : Model -> Element Msg
renderedText model =
    column
        [ spacing 8
        , paddingXY 24 36
        , width (px panelWidth_)
        , height (px (innerPanelHeight model))
        , scrollbarY
        , moveUp 9
        , Font.size 14
        , spacing 14
        , alignTop
        , Background.color (Element.rgb255 255 255 255)
        ]
        (render model.parseData model.count)


latexSourceView : Model -> Element Msg
latexSourceView model =
    column
        [ spacing 8
        , paddingXY 24 36
        , width (px panelWidth_)
        , height (px (innerPanelHeight model))
        , scrollbarY
        , moveUp 9
        , Font.size 14
        , alignTop
        , Background.color (Element.rgb255 255 255 255)
        ]
        [ Element.text (LaTeX.Export.API.export model.language model.sourceText) ]


settings =
    { defaultSettings | paragraphSpacing = 42, showErrorMessages = True }


render : { ast : List Block, accumulator : Accumulator } -> Int -> List (Element Msg)
render parseData count =
    API.renderFancyFromParseData parseData settings count |> List.map (Element.map Render)



-- BUTTONS


defaultButtonColor =
    Element.rgb255 60 60 60


buttonColor buttonMode currentMode =
    if buttonMode == currentMode then
        Element.rgb255 130 12 9

    else
        Element.rgb255 60 60 60


clearTextButton : Element Msg
clearTextButton =
    Input.button buttonStyle2
        { onPress = Just ClearText
        , label = el [ centerX, centerY, Font.size 14 ] (text "Clear")
        }


l1DocButton : Lang -> Element Msg
l1DocButton language =
    Input.button (activeButtonStyle (language == L1))
        { onPress = Just (LoadDocumentText L1 Data.L1Test.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "L1")
        }


markdownDocButton : Lang -> Element Msg
markdownDocButton language =
    Input.button (activeButtonStyle (language == Markdown))
        { onPress = Just (LoadDocumentText Markdown Data.MarkdownTest.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "Markdown")
        }


miniLaTeXDocButton : Lang -> Element Msg
miniLaTeXDocButton language =
    Input.button (activeButtonStyle (language == MiniLaTeX))
        { onPress = Just (LoadDocumentText MiniLaTeX Data.MiniLaTeXTest.text)
        , label = el [ centerX, centerY, Font.size 14 ] (text "MiniLaTeX")
        }


setViewMode : Model -> ViewMode -> Element Msg
setViewMode model viewMode =
    Input.button (activeButtonStyle (viewMode == model.viewMode))
        { onPress = Just (SetViewMode viewMode)
        , label = el [ centerX, centerY, Font.size 14 ] (text (viewModeToString viewMode))
        }


dummyButton : Element Msg
dummyButton =
    row [ Background.color defaultButtonColor ]
        [ Input.button buttonStyle
            { onPress = Nothing
            , label = el [ centerX, centerY, Font.size 14 ] (text "Rendered text")
            }
        ]



--
-- STYLE
--


mainColumnStyle model =
    [ centerX
    , centerY
    , bgGray 0.5
    , paddingXY 20 20
    , width (px (appWidth_ + 40))
    , height (px (appHeight_ model + 40))
    ]


buttonStyle =
    [ Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]


buttonStyle2 =
    [ Font.color (rgb255 255 255 255)
    , Background.color (rgb255 0 0 160)
    , paddingXY 15 8
    , mouseDown [ Background.color (rgb255 180 180 255) ]
    ]


activeButtonStyle isSelected =
    if isSelected then
        [ Font.color (rgb255 255 255 255)
        , Background.color (rgb255 140 0 0)
        , paddingXY 15 8
        , mouseDown [ Background.color (rgb255 255 180 180) ]
        ]

    else
        [ Font.color (rgb255 255 255 255)
        , Background.color (rgb255 0 0 160)
        , paddingXY 15 8
        , mouseDown [ Background.color (rgb255 180 180 255) ]
        ]


grayColor g =
    Element.rgb g g g


whiteColor =
    grayColor 1


fontGray g =
    Font.color (Element.rgb g g g)


bgGray g =
    Background.color (Element.rgb g g g)
