module Markup.Error exposing (Context(..), Problem(..))


type Problem
    = ExpectingPrefix
    | ExpectingSymbol String


type Context
    = TextExpression
