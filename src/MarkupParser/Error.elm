module MarkupParser.Error exposing (Context(..), Problem(..), heading)


type Problem
    = EndOfInput
    | ExpectingPrefix
    | ExpectingSuffix
    | ExpectingSymbol String
    | UnHandledError Int


heading : Problem -> String
heading problem =
    case problem of
        _ ->
            "Error in"


type Context
    = TextExpression
