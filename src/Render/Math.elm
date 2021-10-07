module Render.Math exposing (DisplayMode(..), mathText)

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


mathText : Int -> DisplayMode -> String -> Element msg
mathText generation displayMode content =
    Html.Keyed.node "span"
        [ HA.style "margin-left" "6px" ]
        [ ( String.fromInt generation, mathText_ displayMode "ID" content )
        ]
        |> Element.html


mathText_ : DisplayMode -> String -> String -> Html msg
mathText_ displayMode selectedId content =
    Html.node "math-text"
        -- active meta selectedId  ++
        [ HA.property "display" (Json.Encode.bool (isDisplayMathMode displayMode))
        , HA.property "content" (Json.Encode.string content)

        -- , clicker meta
        -- , HA.id (makeId meta)
        ]
        []


isDisplayMathMode : DisplayMode -> Bool
isDisplayMathMode displayMode =
    case displayMode of
        InlineMathMode ->
            False

        DisplayMathMode ->
            True
