module Render.Math exposing (DisplayMode(..), mathText)

import Element exposing (Element)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Keyed
import Json.Encode


type DisplayMode
    = InlineMathMode
    | DisplayMathMode


mathText : Int -> String -> DisplayMode -> String -> Element msg
mathText generation id displayMode content =
    -- the code 'String.replace "\\ \\" "\\\\"'
    -- is needed because for some reason "\\\\" gets expanded to "\\ \\"
    -- TODO Track this down at the soure.
    Html.Keyed.node "span"
        [ HA.style "margin-left" "6px", HA.style "padding-top" "14px", HA.style "padding-bottom" "14px", HA.id id ]
        [ ( String.fromInt generation, mathText_ displayMode "ID" (content |> String.replace "\\ \\" "\\\\") )
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
