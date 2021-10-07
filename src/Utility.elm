module Utility exposing
    ( elementAttribute
    , keyValueDict
    , prepare
    , takeUntil
    )

import Dict exposing (Dict)
import Element
import Html.Attributes
import Maybe.Extra


keyValueDict : List String -> Dict String String
keyValueDict strings_ =
    List.map (String.split ":") strings_
        |> List.map (List.map String.trim)
        |> List.map pairFromList
        |> Maybe.Extra.values
        |> Dict.fromList


pairFromList : List String -> Maybe ( String, String )
pairFromList strings =
    case strings of
        [ x, y ] ->
            Just ( x, y )

        _ ->
            Nothing


{-| Consider a predicate p and a list of elements
[a\_1, a\_2, ..., a\_n][a_1, a_2, ..., a_n]. Find the unique prefix
[a\_1, a\_2, ..., a\_k][a_1, a_2, ..., a_k] such that the a\_k satisfies p
but the a\_i for i < k do not satisfy p. Return

    > { prefix = [a\_1, ... a\_n][a_1, ... a_n]
    , rest = [a\_{k+1}, ..., a\_n][a_{k+1}, ..., a_n]

    -- EXAMPLE
    > takeUntil (\a -> a == 3) [1,2,3,4,5,6]
    { prefix = [1,2,3], rest = [4,5,6] }

-}
prepare : List String -> List String
prepare strings =
    strings |> List.map reflate |> String.join " " |> String.trim |> String.split "\n"


reflate : String -> String
reflate str =
    if str == "" then
        "\n"

    else
        str


elementAttribute : String -> String -> Element.Attribute msg
elementAttribute key value =
    Element.htmlAttribute (Html.Attributes.attribute key value)


takeUntil : (a -> Bool) -> List a -> State a
takeUntil predicate list =
    loop { rest = list, prefix = [] } (nextState predicate)
        |> (\result -> { result | prefix = List.reverse result.prefix })


type alias State a =
    { rest : List a, prefix : List a }


nextState : (a -> Bool) -> State a -> Step (State a) (State a)
nextState predicate state =
    case List.head state.rest of
        Nothing ->
            Done state

        Just a ->
            if predicate a then
                Done { prefix = a :: state.prefix, rest = List.drop 1 state.rest }

            else
                Loop { prefix = a :: state.prefix, rest = List.drop 1 state.rest }


type Step state a
    = Loop state
    | Done a


loop : state -> (state -> Step state a) -> a
loop s nextState_ =
    case nextState_ s of
        Loop s_ ->
            loop s_ nextState_

        Done b ->
            b
